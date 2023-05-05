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
  alias Zig.Type
  alias Zig.Type.Struct

  import Zig.QuoteErl

  defmacro __before_compile__(context = %{module: module}) do
    # TODO: verify that :otp_app exists
    code_dir = Path.dirname(context.file)
    opts = Module.get_attribute(module, :zigler_opts)

    output =
      module
      |> Module.get_attribute(:zig_code_parts)
      |> Enum.reverse()
      |> Enum.join()
      |> compile(module, code_dir, Keyword.put(opts, :render, :render_elixir))

    if opts[:dump] do
      output |> Macro.to_string() |> Code.format_string!() |> IO.puts()
    end

    output
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
        {:all, overridden} -> create_aliases(nifs: overridden)
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
         sema = Sema.analyze_file!(module, opts),
         new_opts = Keyword.merge(opts, parsed: Zig.Parser.parse(base_code)),
         function_code = precompile(sema, module, assembly_directory, new_opts),
         true <- compiled do
      # parser should only operate on parsed, valid zig code.
      Command.compile(module, new_opts)
      apply(__MODULE__, renderer, [base_code, function_code, module, opts])
    else
      false ->
        apply(__MODULE__, renderer, [base_code, [], module, opts])
    end
  end

  defp precompile(sema, module, directory, opts) do
    verify_sound!(sema, opts)

    nif_functions =
      sema
      |> Nif.from_sema(opts[:nifs])
      |> remove_ignored(opts[:ignore])
      |> assimilate_common_options(opts)

    renderer = Keyword.fetch!(opts, :render)

    function_code = Enum.map(nif_functions, &apply(Nif, renderer, [&1]))

    nif_src_path = Path.join(directory, "nif.zig")

    resource_opts = Keyword.get(opts, :resources, [])

    File.write!(nif_src_path, Nif.render_zig_code(nif_functions, resource_opts, module))
    Command.fmt(nif_src_path)

    Logger.debug("wrote nif.zig to #{nif_src_path}")

    function_code
  end

  def render_elixir(code, function_code, module, opts) do
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
      Zig.Manifest.resolver(unquote(code))

      def _format_error(_, [{_, _, _, opts} | _rest] = _stacktrace) do
        if formatted = opts[:zigler_error], do: formatted, else: %{}
      end
    end
  end

  def render_erlang(_code, function_code, module, opts) do
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

    Enum.flat_map(function_code, &Function.identity/1) ++ init_function
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

  defp verify_sound!(sema, opts) do
    Enum.each(sema, fn function ->
      raise_if_uses_private_struct!(function, opts)
      Type.Function.validate!(function)
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

  @common_options [:leak_check]
  defp assimilate_common_options(nifs, opts) when is_list(nifs) do
    for nif <- nifs do
      %{nif | function: assimilate_common_options(nif.function, opts)}
    end
  end

  defp assimilate_common_options(nif, module_opts) when is_struct(nif) do
    new_opts =
      module_opts
      |> Keyword.take(@common_options)
      |> Keyword.merge(nif.opts)

    %{nif | opts: new_opts}
  end

  defp remove_ignored(functions, ignored) do
    ignored = List.wrap(ignored)
    Enum.reject(functions, &(&1.function.name in ignored))
  end
end
