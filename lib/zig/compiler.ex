defmodule Zig.Compiler do
  @moduledoc """
  handles instrumenting elixir code with hooks for zig NIFs.
  """

  @enforce_keys [:assembly_dir, :code_file, :assembly, :module_spec]

  # contains critical information for the compilation.
  defstruct @enforce_keys ++ [test_dirs: []]

  alias Zig.Assembler

  @type t :: %__MODULE__{
    assembly_dir: Path.t,
    assembly:     [Assembler.t],
    code_file:    Path.t,
    module_spec:  Zig.Module.t
  }

  require Logger

  alias Zig.Command

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)

  #@release_mode %{
  #  fast:  ["--release-fast"],
  #  safe:  ["--release-safe"],
  #  small: ["--release-small"],
  #  debug: []
  #}

  defmacro __before_compile__(context) do

    ###########################################################################
    # VERIFICATION

    module = Module.get_attribute(context.module, :zigler)

    zig_tree = Path.join(@zig_dir_path, Command.version_name(module.zig_version))

    # check to see if the zig version has been downloaded.  If not,
    # go ahead and download it.
    unless File.dir?(zig_tree) do
      Command.fetch("#{module.zig_version}")
    end

    if module.nifs == [] do
      raise CompileError,
        file: context.file,
        description: "no nifs found in the module #{context.module}"
    end

    ###########################################################################
    # COMPILATION STEPS

    compiler = precompile(module)
    unless module.dry_run do
      Command.compile(compiler, zig_tree)
    end
    cleanup(compiler)

    ###########################################################################
    # MACRO STEPS

    dependencies = dependencies_for(compiler.assembly)
    nif_functions = Enum.map(module.nifs, &function_skeleton/1)
    nif_name = Zig.nif_name(module, false)

    if module.dry_run do
      quote do
        unquote_splicing(dependencies)
        unquote_splicing(nif_functions)
        def __load_nifs__, do: :ok
      end
    else
      quote do
        import Logger
        unquote_splicing(dependencies)
        unquote_splicing(nif_functions)
        def __load_nifs__ do
          # LOADS the nifs from :code.lib_dir() <> "ebin", which is
          # a path that has files correctly moved in to release packages.

          unquote(module.otp_app)
          |> :code.lib_dir()
          |> Path.join("ebin")
          |> Path.join(unquote(nif_name))
          |> String.to_charlist()
          |> :erlang.load_nif(0)
          |> case do
            :ok ->
              Logger.debug("loaded module at #{unquote(nif_name)}")
            error = {:error, any} ->
              Logger.error("loading module #{unquote(nif_name)} #{inspect any}")
          end
        end
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

  #############################################################################
  ## FUNCTION SKELETONS

  alias Zig.Nif.{DirtyCpu, DirtyIO, Synchronous, Test, Threaded, Yielding}
  alias Zig.Parser.Nif

  def function_skeleton(nif = %Nif{doc: doc}) when not is_nil(doc) do
    quote do
      @doc unquote(doc)
      unquote(function_skeleton(%{nif | doc: nil}))
    end
  end
  def function_skeleton(nif = %Nif{opts: opts, test: nil}) do
    case opts[:concurrency] do
      :threaded ->
        Threaded.beam_adapter(nif)
      :yielding ->
        Yielding.beam_adapter(nif)
      :dirty_cpu ->
        DirtyCpu.beam_adapter(nif)
      :dirty_io ->
        DirtyIO.beam_adapter(nif)
      nil ->
        Synchronous.beam_adapter(nif)
    end
  end
  def function_skeleton(nif) do
    Test.beam_adapter(nif)
  end

  #############################################################################
  ## STEPS

  def assembly_dir(env, module) do
    Path.join(System.tmp_dir!(), ".zigler_compiler/#{env}/#{module}")
  end

  @spec precompile(Zig.Module.t) :: t | no_return
  def precompile(module) do
    # build the staging directory.
    assembly_dir = assembly_dir(Mix.env, module.module)
    File.mkdir_p!(assembly_dir)

    # create the main code file
    code_file = Path.join(assembly_dir, "#{module.module}.zig")
    code_content = Zig.Code.generate_main(%{module | zig_file: code_file})

    # define the code file and build it.
    File.write!(code_file, code_content)

    # parse the module code to generate the full list of assets
    # that need to be brought in to the assembly directory
    assembly = Assembler.parse_code(code_content,
      parent_dir: Path.dirname(module.file),
      target_dir: assembly_dir,
      pub: true,
      context: [])

    Assembler.assemble_kernel!(assembly_dir)
    Assembler.assemble_assets!(assembly, assembly_dir)

    %__MODULE__{
      assembly_dir: assembly_dir,
      assembly:     assembly,
      code_file:    code_file,
      module_spec:  module,
    }
  end

  @spec cleanup(t) :: :ok | no_return
  defp cleanup(compiler) do
    # in dev and test we keep our code around for debugging purposes.
    unless Mix.env in [:dev, :test] do
      File.rm_rf!(compiler.assembly_dir)
    end
    :ok
  end
end
