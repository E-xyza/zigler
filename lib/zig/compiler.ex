defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  # TODO: refactor to use a struct

  require Logger

  alias Zig.Builder
  alias Zig.Command
  alias Zig.Manifest
  alias Zig.Parser
  alias Zig.Sema
  alias Zig.Manifest

  defmacro __before_compile__(%{module: module, file: file}) do
    # NOTE: this is going to be called only from Elixir.  Erlang will not call this.
    # all functionality in this macro must be replicated when running compilation from
    # erlang.

    opts =
      module
      |> Module.get_attribute(:zigler_opts)
      |> adjust_elixir_options

    code_dir = opts.dir || Path.dirname(file)

    module
    |> Module.get_attribute(:zig_code_parts)
    |> Enum.reverse()
    |> IO.iodata_to_binary()
    |> compile(code_dir, opts)
    |> Zig.Macro.inspect(opts)
  end

  defp adjust_elixir_options(opts) do
    Map.update!(opts, :nifs, &nif_substitution/1)
  end

  # if the elixir `nif` option contains `...` then this should be converted 
  # into `{:auto, <other_options>}`.  Also, if the nif entry is just an atom,
  # converts that entry into `{nif, []}`
  #
  # This function will reverse the list, but since order doesn't matter for this 
  # option, it is okay.
  defp nif_substitution({:auto, _} = auto), do: auto

  defp nif_substitution(opts) do
    Enum.reduce(opts, [], fn
      {:..., _, _}, {:auto, list} = so_far -> so_far
      {:..., _, _}, list -> {:auto, list}
      other, so_far -> prepend_nif(so_far, other)
    end)
  end

  defp prepend_nif({:auto, so_far}, nif_name) when is_atom(nif_name),
    do: {:auto, [{nif_name, []} | so_far]}

  defp prepend_nif(so_far, nif_name) when is_atom(nif_name), do: [{nif_name, []} | so_far]
  defp prepend_nif({:auto, so_far}, nif_info), do: {:auto, [nif_info | so_far]}
  defp prepend_nif(so_far, nif_info), do: [nif_info | so_far]

  # note that this function is made public so that it can be both accessed
  # from the :zigler entrypoint for erlang parse transforms, as well as the
  # __before_compile__ entrypoint for Elixir
  def compile(zig_code, code_dir, opts) do
    zig_code_path = Path.join(code_dir, ".#{opts.module}.zig")

    opts
    |> Map.replace!(:zig_code_path, zig_code_path)
    |> tap(&write_code!(&1, zig_code))
    |> Builder.stage()
    |> Manifest.create(zig_code)
    |> Sema.run_sema!()
    |> apply_parser(zig_code)
    |> Sema.analyze_file!()
    |> tap(&precompile/1)
    |> Command.compile!()
    |> unload_manifest_module()
    |> tap(&Module.put_attribute(opts.module, :zigler_opts, &1))
    |> case do
      %{language: Elixir} = module ->
        Zig.Module.render_elixir(module, zig_code)

      %{language: :erlang} = module ->
        Zig.Module.render_erlang(module, zig_code)
    end
    |> Zig.Macro.inspect(opts)
  end

  defp write_code!(module, zig_code) do
    File.write!(module.zig_code_path, zig_code)
  end

  defp apply_parser(module, zig_code) do
    %{module | parsed: Parser.parse(zig_code)}
  end

  defp precompile(module) do
    path =
      module.module
      |> Builder.staging_directory()
      |> Path.join("module.zig")

    File.write!(path, Zig.Module.render_zig(module))
    Command.fmt(path)

    Logger.debug("wrote module code to #{path}")
  end

  # require EEx
  # zig_alias_template = Path.join(__DIR__, "templates/alias.zig.eex")
  # EEx.function_from_file(:defp, :create_aliases, zig_alias_template, [:assigns])

  # defp dependencies_for(assemblies) do
  #  Enum.map(assemblies, fn assembly ->
  #    quote do
  #      @external_resource unquote(assembly.source)
  #    end
  #  end)
  # end

  #############################################################################
  ## STEPS

  def assembly_dir(env, module) do
    System.tmp_dir()
    |> String.replace("\\", "/")
    |> Path.join(".zigler_compiler/#{env}/#{module}")
  end

  def unload_manifest_module(module) do
    :code.soft_purge(module.manifest_module) || raise "manifest purging error"
    module
  end
end
