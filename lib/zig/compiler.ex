defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  # TODO: refactor to use a struct

  require Logger

  alias Zig.Assembler
  alias Zig.Command
  alias Zig.EasyC
  alias Zig.Manifest
  alias Zig.Nif
  alias Zig.Options
  alias Zig.Sema
  alias Zig.Manifest

  defmacro __before_compile__(%{module: module, file: file}) do
    # NOTE: this is going to be called only from Elixir.  Erlang will not call this.
    # all functionality in this macro must be replicated when running compilation from
    # erlang.

    # TODO: verify that :otp_app exists
    code_dir = Path.dirname(file)

    opts =
      module
      |> Module.get_attribute(:zigler_opts)
      |> Options.normalize!()

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
         assemble_opts =
           Keyword.take(opts, [
             :link_lib,
             :build_opts,
             :stage1,
             :include_dir,
             :c_src,
             :packages,
             :local_zig
           ]),
         assemble_opts = Keyword.merge(assemble_opts, from: code_dir),
         Assembler.assemble(module, assemble_opts),
         true <- precompiled,
         # TODO: verify that this parsed correctly.
         manifest = Manifest.create(full_code),
         new_opts = Keyword.merge(opts, manifest: manifest),
         file = Keyword.fetch!(opts, :file),
         sema_result = Sema.run_sema!(file, module, new_opts),
         parsed_code = Zig.Parser.parse(base_code),
         new_opts = Keyword.merge(new_opts, parsed: parsed_code),
         sema_nifs = Sema.analyze_file!(sema_result, new_opts),
         new_opts = Keyword.put(new_opts, :nifs, sema_nifs),
         function_code = precompile(module, assembly_directory, new_opts),
         {true, _} <- {compiled, new_opts} do
      # parser should only operate on parsed, valid zig code.
      Command.compile(module, new_opts)
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

    nif_functions =
      opts
      |> Keyword.fetch!(:nifs)
      |> Enum.map(fn {name, nif_opts} ->
        doc =
          opts
          |> Keyword.fetch!(:parsed)
          |> Map.fetch!(:code)
          |> Enum.find_value(fn
            %{name: ^name, doc_comment: doc_comment} -> doc_comment
            _ -> nil
          end)

        Nif.new(name, Keyword.put(nif_opts, :doc, doc))
      end)

    function_code = Enum.map(nif_functions, &apply(Nif, render_fn, [&1]))

    nif_src_path = Path.join(directory, "module.zig")

    resource_opts = Keyword.get(opts, :resources, [])
    callbacks = Keyword.get(opts, :callbacks)

    File.write!(
      nif_src_path,
      Zig.Module.render_zig(nif_functions, resource_opts, callbacks, module)
    )

    Command.fmt(nif_src_path, opts)

    Logger.debug("wrote module.zig to #{nif_src_path}")

    function_code
  end

  require EEx
  zig_alias_template = Path.join(__DIR__, "templates/alias.zig.eex")
  EEx.function_from_file(:defp, :create_aliases, zig_alias_template, [:assigns])

  #############################################################################
  ## STEPS

  def assembly_dir(env, module) do
    System.tmp_dir()
    |> String.replace("\\", "/")
    |> Path.join(".zigler_compiler/#{env}/#{module}")
  end
end
