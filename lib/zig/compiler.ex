defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  # TODO: refactor to use a struct

  require Logger

  alias Zig.Analyzer
  alias Zig.Assembler
  alias Zig.Command
  alias Zig.EasyC
  alias Zig.Nif
  alias Zig.Sema
  alias Zig.Type.Struct
  alias Zig.Manifest

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
    |> IO.iodata_to_binary()
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

    full_code = IO.iodata_to_binary([aliasing_code, easy_c_code, base_code])

    File.write!(module_nif_zig, full_code)

    if opts[:easy_c] do
      Command.fmt(module_nif_zig)
    end

    assembled = Keyword.get(opts, :assemble, true)
    precompiled = Keyword.get(opts, :precompile, true)
    compiled = Keyword.get(opts, :compile, true)
    renderer = Keyword.fetch!(opts, :render)

    with true <- assembled,
         assemble_opts = Keyword.take(opts, [:link_lib, :build_opts, :stage1]),
         assemble_opts = Keyword.merge(assemble_opts, from: code_dir),
         Assembler.assemble(module, assemble_opts),
         true <- precompiled,
         # TODO: verify that this parsed correctly.
         manifest = Manifest.create(full_code),
         parsed_code = Zig.Parser.parse(base_code),
         new_opts = Keyword.merge(opts, manifest: manifest, parsed: parsed_code),
         sema_nifs = Sema.analyze_file!(module, new_opts),
         new_opts = Keyword.put(new_opts, :nifs, sema_nifs),
         function_code = precompile(module, assembly_directory, new_opts),
         {true, _} <- {compiled, new_opts} do
      # parser should only operate on parsed, valid zig code.
      Command.compile(module, opts)
      apply(Zig.Module, renderer, [base_code, function_code, module, manifest, new_opts])
    else
      false ->
        apply(Zig.Module, renderer, [base_code, [], module, [], opts])

      {false, new_opts} ->
        apply(Zig.Module, renderer, [base_code, [], module, [], new_opts])
    end
  end

  defp precompile(module, directory, opts) do
    render_fn = Keyword.fetch!(opts, :render)
    parsed_code = Keyword.fetch!(opts, :parsed)

    nif_functions =
      opts
      |> Keyword.fetch!(:nifs)
      |> Enum.map(fn {name, opts} ->
        Nif.new(name, opts)
      end)

    function_code = Enum.map(nif_functions, &apply(Nif, render_fn, [&1]))

    nif_src_path = Path.join(directory, "module.zig")

    resource_opts = Keyword.get(opts, :resources, [])
    callbacks = Keyword.get(opts, :callbacks)

    File.write!(
      nif_src_path,
      Zig.Module.render_zig(nif_functions, resource_opts, callbacks, module)
    )

    Command.fmt(nif_src_path)

    Logger.debug("wrote module.zig to #{nif_src_path}")

    function_code
  end

  #  defp assign_positions(opts) do
  #    nif_functions =
  #
  #    if opts[:easy_c] do
  #  #    [mod_file: file, mod_line: line] = Keyword.take(opts, [:mod_file, :mod_line])
  #  #    Enum.map(nif_functions, &%{&1 | file: file, line: line})
  #
  #      # not implemented yet
  #    else
  #      manifest = Keyword.fetch!(opts, :manifest)
  #      file = Keyword.fetch!(opts, :mod_file)
  #
  #      Enum.map(nif_functions, &assign_position(&1, parsed_code, manifest, file))
  #      raise "foo"
  #    end
  #  end
  #
  #  defp assign_position(nifs, code) do
  #    nifs |> dbg(limit: 25)
  #    code |> dbg(limit: 25)
  #    raise "aaa"
  #
  #
  #    #function, %{code: code}, manifest, file) do
  #    #name = function.alias || function.type.name
  ##
  #    #parsed_line =
  #    #  Enum.find_value(code, fn
  #    #    {:fn, %{position: %{line: line}}, vals} ->
  #    #      if Keyword.get(vals, :name) == name, do: line
  ##
  #    #    _ ->
  #    #      false
  #    #  end)
  ##
  #    #line = Manifest.resolve(manifest, parsed_line)
  #    #%{function | file: file, line: line}
  #  end
  #

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

  #  defp verify_soundness!(nif_functions, parsed) do
  #    Enum.map(nif_functions, fn fun = %{type: type} ->
  #      raise_if_uses_private_struct!(type, fun.file, parsed)
  #      Nif.validate_return!(type, fun.file, fun.line)
  #      fun
  #    end)
  #  end
  #
  #  # verifies that none of the structs returned are private.
  #  defp raise_if_uses_private_struct!(function = %{name: name}, file, parsed) do
  #    Enum.each(function.params, &raise_if_private_struct!(&1, file, name, "accepts", parsed))
  #    raise_if_private_struct!(function.return, file, name, "returns", parsed)
  #    function
  #  end
  #
  #  defp raise_if_private_struct!(%Struct{name: name}, file, function_name, verb, parsed) do
  #    case Analyzer.info_for(parsed, name) do
  #      {:const, %{pub: false, position: const_position}, _} ->
  #        {:fn, fn_opts, _} = Analyzer.info_for(parsed, Atom.to_string(function_name))
  #
  #        {fn_file, fn_line} = Analyzer.translate_location(parsed, file, fn_opts.position.line)
  #
  #        {st_file, st_line} = Analyzer.translate_location(parsed, file, const_position.line)
  #
  #        raise CompileError,
  #          file: Path.relative_to_cwd(fn_file),
  #          line: fn_line,
  #          description:
  #            "the function `#{function_name}` #{verb} the struct `#{name}` which is not public (defined at #{st_file}:#{st_line})"
  #
  #      _ ->
  #        :ok
  #    end
  #  end
  #
  #  defp raise_if_private_struct!(_, _, _, _, _), do: :ok
end
