defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  require Logger

  alias Zig.Analyzer
  alias Zig.Assembler
  alias Zig.Command
  alias Zig.EasyC
  alias Zig.Nif
  alias Zig.Sema
  alias Zig.Type.Struct
  alias Zig.Manifest

  import Zig.QuoteErl

  defmacro __before_compile__(%{module: module, file: file}) do
    # NOTE: this is going to be called only from Elixir.  Erlang will not call this.
    # all functionality in this macro must be replicated when running compilation from
    # erlang.

    # TODO: verify that :otp_app exists
    code_dir = Path.dirname(file)
    opts = Module.get_attribute(module, :zigler_opts)

    module
    |> Module.get_attribute(:zig_code_parts)
    |> Enum.reverse()
    |> Enum.join()
    |> compile(module, code_dir, Keyword.put(opts, :render, :render_elixir))
    |> Zig.Macro.inspect(opts)
  end

  # note that this directory is made public so that it can be both accessed
  # from the :zigler entrypoint for erlang parse transforms, as well as the
  # __before_compile__ entrypoint for Elixir
  def compile(base_code, module, code_dir, opts) do
    module_nif_zig = Path.join(code_dir, ".#{module}.zig")
    opts = Keyword.merge(opts, file: module_nif_zig)

    assembly_directory = Assembler.directory(module)

    # obtain the code
    easy_c_code = List.wrap(if opts[:easy_c], do: EasyC.build_from(opts))

    aliasing_code =
      case opts[:nifs] do
        {:auto, overridden} -> create_aliases(nifs: overridden)
        functions when is_list(functions) -> create_aliases(nifs: functions)
      end

    File.write!(module_nif_zig, [aliasing_code, easy_c_code, base_code])

    if opts[:easy_c] do
      Command.fmt(module_nif_zig)
    end

    assembled = Keyword.get(opts, :assemble, true)
    precompiled = Keyword.get(opts, :precompile, true)
    compiled = Keyword.get(opts, :compile, true)
    renderer = Keyword.fetch!(opts, :render)

    with true <- assembled,
         assemble_opts = Keyword.take(opts, [:link_lib, :build_opts]),
         assemble_opts = Keyword.merge(assemble_opts, from: code_dir),
         Assembler.assemble(module, assemble_opts),
         true <- precompiled,
         # TODO: verify that this parsed correctly.
         manifest = Manifest.create(base_code),
         {sema_functions, new_nif_opts} = Sema.analyze_file!(module, manifest, opts),
         parsed_code = Zig.Parser.parse(base_code),
         new_opts =
           Keyword.merge(opts, nifs: new_nif_opts, manifest: manifest, parsed: parsed_code),
         function_code =
           precompile(sema_functions, parsed_code, module, assembly_directory, new_opts),
         {true, _} <- {compiled, new_opts} do
      # parser should only operate on parsed, valid zig code.
      Command.compile(module, opts)
      apply(__MODULE__, renderer, [base_code, function_code, module, manifest, new_opts])
    else
      false ->
        apply(__MODULE__, renderer, [base_code, [], module, [], opts])

      {false, new_opts} ->
        apply(__MODULE__, renderer, [base_code, [], module, [], new_opts])
    end
  end

  defp precompile(sema_functions, parsed_code, module, directory, opts) do
    render_fn = Keyword.fetch!(opts, :render)

    nif_functions =
      sema_functions
      |> Zig.Module.nifs_from_sema(Keyword.fetch!(opts, :nifs))
      |> assign_positions(parsed_code, opts)
      |> verify_soundness!(Keyword.fetch!(opts, :parsed))

    function_code = Enum.map(nif_functions, &apply(Nif, render_fn, [&1]))

    nif_src_path = Path.join(directory, "nif.zig")

    resource_opts = Keyword.get(opts, :resources, [])

    File.write!(nif_src_path, Nif.render_zig(nif_functions, resource_opts, module))
    Command.fmt(nif_src_path)

    Logger.debug("wrote nif.zig to #{nif_src_path}")

    function_code
  end

  defp assign_positions(nif_functions, parsed_code, opts) do
    if opts[:easy_c] do
      [mod_file: file, mod_line: line] = Keyword.take(opts, [:mod_file, :mod_line])
      Enum.map(nif_functions, &%{&1 | file: file, line: line})
    else
      # we need the manifest here.
      manifest = Keyword.fetch!(opts, :manifest)
      file = Keyword.fetch!(opts, :mod_file)
      Enum.map(nif_functions, &assign_position(&1, parsed_code, manifest, file))
    end
  end

  defp assign_position(function, %{code: code}, manifest, file) do
    name = function.alias || function.type.name

    parsed_line =
      Enum.find_value(code, fn
        {:fn, %{position: %{line: line}}, vals} ->
          if Keyword.get(vals, :name) == name, do: line

        _ ->
          false
      end)

    line = Manifest.resolve(manifest, parsed_line)
    %{function | file: file, line: line}
  end

  def render_elixir(code, function_code, module, manifest, opts) do
    nif_name = "#{module}"

    load_nif_fn =
      case function_code do
        [] ->
          logger_msg = "module #{inspect(module)} does not compile its nifs"

          quote do
            def __load_nifs__ do
              require Logger
              Logger.info(unquote(logger_msg))
            end
          end

        _ ->
          quote do
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
      end

    quote do
      @zig_code unquote(code)

      unquote_splicing(function_code)

      unquote(load_nif_fn)

      require Zig.Manifest
      Zig.Manifest.resolver(unquote(manifest))

      def _format_error(_, [{_, _, _, opts} | _rest] = _stacktrace) do
        if formatted = opts[:zigler_error], do: formatted, else: %{}
      end
    end
  end

  def render_erlang(_code, function_code, module, _manifest, opts) do
    otp_app = Keyword.fetch!(opts, :otp_app)
    module_name = Atom.to_charlist(module)

    init_function =
      quote_erl(
        """
        '__init__'() ->
          erlang:load_nif(filename:join(code:priv_dir(unquote(otp_app)), unquote(module_id)), []).
        """,
        otp_app: otp_app,
        module_id: ~C'lib/' ++ module_name
      )

    Enum.flat_map(function_code, & &1) ++ init_function
  end

  require EEx
  zig_alias_template = Path.join(__DIR__, "templates/alias.zig.eex")
  EEx.function_from_file(:defp, :create_aliases, zig_alias_template, [:assigns])

  defp dependencies_for(assemblies) do
    Enum.map(assemblies, fn assembly ->
      quote do
        @external_resource unquote(assembly.source)
      end
    end)
  end

  #############################################################################
  ## STEPS

  def assembly_dir(env, module) do
    System.tmp_dir()
    |> String.replace("\\", "/")
    |> Path.join(".zigler_compiler/#{env}/#{module}")
  end

  defp verify_soundness!(nif_functions, parsed) do
    Enum.map(nif_functions, fn fun = %{type: type} ->
      raise_if_uses_private_struct!(type, fun.file, parsed)
      Nif.validate_return!(type, fun.file, fun.line)
      fun
    end)
  end

  # verifies that none of the structs returned are private.
  defp raise_if_uses_private_struct!(function = %{name: name}, file, parsed) do
    Enum.each(function.params, &raise_if_private_struct!(&1, file, name, "accepts", parsed))
    raise_if_private_struct!(function.return, file, name, "returns", parsed)
    function
  end

  defp raise_if_private_struct!(%Struct{name: name}, file, function_name, verb, parsed) do
    case Analyzer.info_for(parsed, name) do
      {:const, %{pub: false, position: const_position}, _} ->
        {:fn, fn_opts, _} = Analyzer.info_for(parsed, Atom.to_string(function_name))

        {fn_file, fn_line} = Analyzer.translate_location(parsed, file, fn_opts.position.line)

        {st_file, st_line} = Analyzer.translate_location(parsed, file, const_position.line)

        raise CompileError,
          file: Path.relative_to_cwd(fn_file),
          line: fn_line,
          description:
            "the function `#{function_name}` #{verb} the struct `#{name}` which is not public (defined at #{st_file}:#{st_line})"

      _ ->
        :ok
    end
  end

  defp raise_if_private_struct!(_, _, _, _, _), do: :ok
end
