defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  require Logger

  alias Zig.Analyzer
  alias Zig.Assembler
  alias Zig.Command
  alias Zig.Nif
  alias Zig.Sema
  alias Zig.Type.Struct

  defmacro __before_compile__(context) do
    # TODO: verify that :otp_app exists
    this_dir = Path.dirname(context.file)
    module_nif_zig = Path.join(this_dir, ".#{context.module}.zig")

    # obtain the code
    code =
      context.module
      |> Module.get_attribute(:zig_code_parts)
      |> Enum.reverse()
      |> Enum.join()

    File.write!(module_nif_zig, code)

    opts =
      context.module
      |> Module.get_attribute(:zigler_opts)
      |> Keyword.merge(file: module_nif_zig)

    assembled = Keyword.get(opts, :assemble, true)
    precompiled = Keyword.get(opts, :precompile, true)
    compiled = Keyword.get(opts, :compile, true)

    directory = Assembler.directory(context.module)

    with true <- assembled,
         Assembler.assemble(context.module, from: this_dir),
         true <- precompiled,
         sema = Sema.analyze_file!(context.module, opts),
         new_opts = Keyword.merge(opts, parsed: Zig.Parser.parse(code)),
         function_code = precompile(sema, context, directory, new_opts),
         true <- compiled do
      # parser should only operate on parsed, valid zig code.
      Command.compile(context.module, new_opts)

      nif_name = "#{context.module}"

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

        def _format_error(_, [{_, _, _, opts} | _rest] = _stacktrace) do
          if formatted = opts[:zigler_error], do: formatted, else: %{}
        end
      end
    else
      false ->
        dead_module(code, context)
    end
  end

  defp precompile(sema, context, directory, opts) do
    verify_sound!(sema, opts)

    nif_functions = Nif.from_sema(sema, opts[:nifs])

    function_code = Enum.map(nif_functions, &Nif.render_elixir/1)

    directory
    |> Path.join("nif.zig")
    |> File.write!(Nif.render_zig(nif_functions, context.module))

    function_code
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

  defp zig_location(zig_tree, module, local_zig \\ @local_zig)

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

  #############################################################################
  ## STEPS

  def assembly_dir(env, module) do
    System.tmp_dir()
    |> String.replace("\\", "/")
    |> Path.join(".zigler_compiler/#{env}/#{module}")
  end

  defp verify_sound!(sema, opts) do
    Enum.each(sema, fn function ->
      raise_if_uses_private_struct!(function, opts)
    end)
  end

  # verifies that none of the structs returned are private.
  defp raise_if_uses_private_struct!(function = %{name: name}, opts) do
    Enum.each(function.params, &raise_if_private_struct!(&1, name, "accepts", opts))
    raise_if_private_struct!(function.return, name, "returns", opts)
    function
  end

  defp raise_if_private_struct!(%Struct{name: name}, function_name, verb, opts) do
    parsed = opts[:parsed]

    case Analyzer.info_for(parsed, name) do
      {:const, %{pub: false, position: const_position}, _} ->
        {:fn, fn_opts, _} = Analyzer.info_for(parsed, Atom.to_string(function_name))

        {fn_file, fn_line} =
          Analyzer.translate_location(parsed, opts[:file], fn_opts.position.line)

        {st_file, st_line} = Analyzer.translate_location(parsed, opts[:file], const_position.line)

        raise CompileError,
          file: fn_file,
          line: fn_line,
          description:
            "the function `#{function_name}` #{verb} the struct `#{name}` which is not public (defined at #{st_file}:#{st_line})"

      _ ->
        :ok
    end
  end

  defp raise_if_private_struct!(_, _, _, _), do: :ok
end
