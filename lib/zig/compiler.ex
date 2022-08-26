defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  require Logger

  alias Zig.Assembler
  alias Zig.Command
  alias Zig.Nif
  alias Zig.Sema

  defmacro __before_compile__(context) do
    # TODO: verify that :otp_app exists

    opts = Module.get_attribute(context.module, :zigler_opts)

    assembled = Keyword.get(opts, :assemble, true)
    precompiled = Keyword.get(opts, :precompile, true)
    compiled = precompiled and Keyword.get(opts, :compile, true)

    _ = assembled

    this_dir = Path.dirname(context.file)
    module_nif_zig = Path.join(this_dir, ".#{context.module}.zig")

    # obtain the code
    code =
      context.module
      |> Module.get_attribute(:zig_code_parts)
      |> Enum.reverse()
      |> Enum.join()

    File.write!(module_nif_zig, code)

    directory = Assembler.directory(context.module)

    _ = directory

    if assembled do
      # TODO: this should do all the things: build out the nif.zig and sema.zig files.
      # should also make a cnode.zig (if applicable)
      Assembler.assemble(context.module, from: this_dir)

      if precompiled do
        sema = Sema.analyze_file!(context.module, opts)

        nif_functions = Nif.from_sema(sema, opts[:nifs])

        function_code = Enum.map(nif_functions, &Nif.render_elixir/1)

        nif_name = "#{context.module}"

        directory
        |> Path.join("nif.zig")
        |> File.write!(Nif.render_zig(nif_functions, context.module))

        if compiled do
          Command.compile(context.module, opts)

          quote do
            @zig_code unquote(code)

            unquote_splicing(function_code)

            def __load_nifs__ do
              # LOADS the nifs from :code.lib_dir() <> "ebin", which is
              # a path that has files correctly moved in to release packages.

              require Logger

              unquote(opts[:otp_app])
              |> :code.lib_dir()
              |> Path.join("ebin/lib")
              |> Path.join(unquote(nif_name))
              |> String.to_charlist()
              |> :erlang.load_nif(0)
              |> case do
                :ok ->
                  Logger.debug("loaded module at #{unquote(nif_name)}")

                error = {:error, any} ->
                  Logger.error("loading module #{unquote(nif_name)} #{inspect(any)}")
              end
            end
          end
        else
          dead_module(code, context)
        end
      else
        dead_module(code, context)
      end
    else
      dead_module(code, context)
    end
  end

  defp dead_module(code, context) do
    logger_msg = "module #{inspect(context.module)} does not compile its nifs"

    quote do
      @zig_code unquote(code)
      def __load_nifs__ do
        require Logger
        Logger.info(unquote(logger_msg))
      end
    end
  end

  defp dependencies_for(assemblies) do
    Enum.map(assemblies, fn assembly ->
      quote do
        @external_resource unquote(assembly.source)
      end
    end)
  end

  @local_zig Application.compile_env(:zigler, :local_zig, false)

  defp zig_location(zig_tree, module), do: zig_location(zig_tree, module, @local_zig)

  defp zig_location(_, _, true), do: System.find_executable("zig")

  defp zig_location(zig_tree, module, false) do
    # check to see if the zig version has been downloaded.  If not,
    # go ahead and download it.
    unless File.dir?(zig_tree) do
      Command.fetch("#{module.zig_version}")
    end

    Path.join(zig_tree, "zig")
  end

  defp zig_location(_, _, path), do: path

  defp resolve(zig_path) do
    Path.dirname(
      with {:ok, %{type: :link}} <- File.lstat(zig_path),
           {:ok, path} <- File.read_link(zig_path) do
        path
      else
        _ -> zig_path
      end
    )
  end

  # credo:disable-for-next-line
  defp exception_for(mod = %{nifs: nifs}) do
    if Enum.any?(nifs, &returns_error?/1) do
      # raise a compile error, if we are running linux and we don't link_libc
      if {:unix, :linux} == :os.type() and not mod.link_libc do
        raise CompileError,
          file: mod.file,
          description:
            "a nif module that has zig error returns compiled with debug symbols must be `link_libc: true`"
      end

      quote do
        defmodule ZigError do
          defexception [:message, :error_return_trace]

          def blame(exception, stacktrace) do
            [{m, f, _, _} | _] = stacktrace
            [%{code: code, nifs: nifs}] = m.__info__(:attributes)[:zigler]
            a = Enum.find(nifs, &(&1.name == f)).arity

            new_message = "#{inspect(m)}.#{f}/#{a} returned the zig error `.#{exception.message}`"

            zig_errors =
              Enum.map(exception.error_return_trace, &process_return_trace(m, code, &1))

            {%{exception | message: new_message}, Enum.reverse(zig_errors, stacktrace)}
          end

          defp process_return_trace(m, code, {_, fun, error_file, error_line}) do
            code_map = m.__info__(:attributes)[:nif_code_map]
            [zig_root] = m.__info__(:attributes)[:zig_root_dir]

            cond do
              String.starts_with?(error_file, zig_root) ->
                file = String.replace_leading(error_file, zig_root, "[zig]")

                {:@, fun, [:...], [file: file, line: error_line]}

              String.starts_with?(Path.basename(error_file), "#{m}") ->
                {src_file, src_line} =
                  code
                  |> IO.iodata_to_binary()
                  |> String.split("\n")
                  |> line_lookup(error_line)

                {:.., fun, [:...], [file: src_file, line: src_line]}

              lookup = List.keyfind(code_map, error_file, 0) ->
                {_, src_file} = lookup

                {:.., fun, [:...], [file: src_file, line: error_line]}

              true ->
                {:.., fun, [:...], [file: error_file, line: error_line]}
            end
          end

          defp stack_sig(fun, m, error_file, error_line, zig_root, code, code_map) do
            cond do
              String.starts_with?(error_file, zig_root) ->
                file = String.replace_leading(error_file, zig_root, "[zig]")

                {:@, fun, [:...], [file: file, line: error_line]}

              String.starts_with?(Path.basename(error_file), "#{m}") ->
                {src_file, src_line} =
                  code
                  |> IO.iodata_to_binary()
                  |> String.split("\n")
                  |> line_lookup(error_line)

                {:.., fun, [:...], [file: src_file, line: src_line]}

              lookup = List.keyfind(code_map, error_file, 0) ->
                {_, src_file} = lookup

                {:.., fun, [:...], [file: src_file, line: error_line]}

              true ->
                {:.., fun, [:...], [file: error_file, line: error_line]}
            end
          end

          defp file_lookup(code_map, dest_file) do
            :proplists.get_value(dest_file, code_map, dest_file)
          end

          defp line_lookup(code_cache, dest_line) do
            code_cache
            |> Enum.with_index(1)
            |> Enum.reduce({"", 0}, fn
              {_, ^dest_line}, fileline ->
                throw fileline

              {"// ref: " <> spec, _}, _ ->
                [file, "line:", line] = String.split(spec)
                {file, String.to_integer(line) + 1}

              _, {file, line} ->
                {file, line + 1}
            end)
          catch
            fileline -> fileline
          end
        end
      end
    end
  end

  defp returns_error?(%{retval: retval}) do
    match?("!" <> _, retval)
  end

  #############################################################################
  ## STEPS

  def assembly_dir(env, module) do
    System.tmp_dir()
    |> String.replace("\\", "/")
    |> Path.join(".zigler_compiler/#{env}/#{module}")
  end
end
