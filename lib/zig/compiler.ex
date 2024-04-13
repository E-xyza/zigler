defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  # TODO: refactor to use a struct

  require Logger

  alias Zig.Builder
  alias Zig.Command
  alias Zig.EasyC
  alias Zig.Manifest
  alias Zig.Nif
  alias Zig.Parser
  alias Zig.Sema
  alias Zig.Manifest

  defmacro __before_compile__(%{module: module, file: file}) do
    # NOTE: this is going to be called only from Elixir.  Erlang will not call this.
    # all functionality in this macro must be replicated when running compilation from
    # erlang.

    code_dir = Path.dirname(file)

    opts = module
    |> Module.get_attribute(:zigler_opts)
    |> adjust_elixir_options

    module
    |> Module.get_attribute(:zig_code_parts)
    |> Enum.reverse()
    |> IO.iodata_to_binary()
    |> compile(code_dir, opts)
    |> Zig.Macro.inspect(opts)
  end

  defp adjust_elixir_options(opts) do
    Map.update!(opts, :nifs, &replace_nif_dots/1)
  end
  
  # if the elixir `nif` option contains `...` then this should be converted 
  # into `{:auto, <other_options>}`.  This function will reverse the list, but
  # since order doesn't matter for this option, it is okay.
  defp replace_nif_dots(opts) do
    Enum.reduce(opts, [], fn 
      {:..., _, _}, {:auto, list} -> {:auto, list}
      {:..., _, _}, list -> {:auto, list}
      other, {:auto, list} -> {:auto, [other | list]}
      other, list -> [other | list]
    end)
  end

  # note that this function is made public so that it can be both accessed
  # from the :zigler entrypoint for erlang parse transforms, as well as the
  # __before_compile__ entrypoint for Elixir
  def compile(base_code, code_dir, opts) do
    nif_code_path = Path.join(code_dir, ".#{opts.module}.zig")

    opts
    |> Map.put(:nif_code_path, nif_code_path)
    |> tap(&Builder.build/1)
    |> Manifest.create(base_code)
    |> Sema.run_sema!()
    |> apply_parser(base_code)
    |> Sema.analyze_file!()
    |> precompile
    |> Command.compile!()
    |> case do
      %{language: Elixir} = opts ->
        Zig.Module.render_elixir(opts)

      %{language: :erlang} = opts ->
        Zig.Module.render_erlang(opts)
    end

    # assembly_directory = Assembler.directory(module)
    #
    ## obtain the code
    # easy_c_code = List.wrap(if opts[:easy_c], do: EasyC.build_from(opts))
    #
    # aliasing_code =
    #  case opts[:nifs] do
    #    {:auto, overridden} -> create_aliases(nifs: overridden)
    #    functions when is_list(functions) -> create_aliases(nifs: functions)
    #  end
    #
    # full_code = IO.iodata_to_binary([aliasing_code, easy_c_code, base_code])
    #
    # File.write!(module_nif_zig, full_code)
    #
    # if opts[:easy_c] do
    #  Command.fmt(module_nif_zig)
    # end

    # with 
    #     Assembler.assemble(module, assemble_opts),
    #     # TODO: verify that this parsed correctly.
    #     manifest = Manifest.create(full_code),
    #     new_opts = Keyword.merge(opts, manifest: manifest),
    #     file = Keyword.fetch!(opts, :file),
    #     sema_result = Sema.run_sema!(file, module, new_opts),
    #     parsed_code = Zig.Parser.parse(base_code),
    #     new_opts = Keyword.merge(new_opts, parsed: parsed_code),
    #     sema_nifs = Sema.analyze_file!(sema_result, new_opts),
    #     new_opts = Keyword.put(new_opts, :nifs, sema_nifs),
    #     function_code = precompile(module, assembly_directory, new_opts),
    #     {true, _} <- {compiled, new_opts} do
    #  # parser should only operate on parsed, valid zig code.
    #  Command.compile(module, new_opts)
    #  apply(Zig.Module, renderer, [base_code, function_code, module, manifest, new_opts])
    # else
    #  false ->
    #    apply(Zig.Module, renderer, [base_code, [], module, [], opts])
    #  {false, new_opts} ->
    #    apply(Zig.Module, renderer, [base_code, [], module, [], new_opts])
    # end
  end

  defp apply_parser(module, base_code) do
    %{module | parsed: Parser.parse(base_code)}
  end

  defp precompile(%{nif_code_path: path} = module) do
    File.write!(path, Zig.Module.render_zig(module))
    Command.fmt(path)
    Logger.debug("wrote module code to #{path}")
    module
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
end
