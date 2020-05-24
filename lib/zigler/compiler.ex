defmodule Zigler.Compiler do

  @moduledoc false

  @enforce_keys [:assembly_dir, :code_file, :assembly, :module_spec]

  # contains critical information for the compilation.
  defstruct @enforce_keys ++ [test_dirs: []]

  alias Zigler.Assembler

  @type t :: %__MODULE__{
    assembly_dir: Path.t,
    assembly:     [Assembler.t],
    code_file:    Path.t,
    module_spec:  Zigler.Module.t
  }

  require Logger

  alias Zigler.Zig

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)

  @doc false
  def basename(version) do
    os = case :os.type do
      {:unix, :linux} ->
        "linux"
      {:unix, :freebsd} ->
        "freebsd"
      {:unix, :darwin} ->
        Logger.warn("macos support is experimental")
        "macos"
      {:win32, _} ->
        Logger.error("windows is definitely not supported.")
        "windows"
    end
    "zig-#{os}-x86_64-#{version}"
  end

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

    zig_tree = Path.join(@zig_dir_path, basename(module.zig_version))

    # check to see if the zig version has been downloaded.
    unless File.dir?(zig_tree) do
      raise CompileError,
        file: context.file,
        line: context.line,
        description: "zig hasn't been downloaded.  Run mix zigler.get_zig #{module.zig_version}"
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
      Zig.compile(compiler, zig_tree)
    end
    cleanup(compiler)

    ###########################################################################
    # MACRO SETPS

    nif_functions = Enum.map(module.nifs, &function_skeleton/1)
    nif_name = Zigler.nif_name(module, false)

    if module.dry_run do
      quote do
        unquote_splicing(nif_functions)
        def __load_nifs__, do: :ok
      end
    else
      quote do
        import Logger
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
              Logger.info("loaded module at #{unquote(nif_name)}")
            error = {:error, any} ->
              Logger.error("loading module #{unquote(nif_name)} #{inspect any}")
          end
        end
      end
    end
  end

  #############################################################################
  ## FUNCTION SKELETONS

  alias Zigler.Code.LongRunning
  alias Zigler.Parser.Nif
  alias Zigler.Typespec

  def function_skeleton(nif = %Nif{opts: opts, test: nil}) do
    typespec = Typespec.from_nif(nif)
    if opts[:long] do
      {:__block__, _, block_contents} = LongRunning.function_skeleton(nif)
      quote do
        unquote(typespec)
        unquote_splicing(block_contents)
      end
    else
      quote do
        unquote(typespec)
        unquote(basic_fn(nif))
      end
    end
  end
  def function_skeleton(%Nif{test: test}) do
    raise_msg = "nif for test #{test} not found"
    raise_code = quote do
      raise unquote(raise_msg)
    end
    {:def, [context: Elixir, import: Kernel],
      [
        {test, [context: Elixir], Elixir},
        [do: raise_code]
      ]}
  end

  defp basic_fn(%{name: name, arity: arity}) do
    text = "nif for function #{name}/#{arity} not bound"

    args = if arity == 0 do
      Elixir
    else
      for _ <- 1..arity, do: {:_, [], Elixir}
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {name, [context: Elixir], args},
        [do: {:raise, [context: Elixir, import: Kernel], [text]}]
      ]}
  end

  #############################################################################
  ## STEPS

  @spec precompile(Zigler.Module.t) :: t | no_return
  def precompile(module) do
    # build the staging directory.
    assembly_dir = Path.join(System.tmp_dir!(),
      ".zigler_compiler/#{Mix.env}/#{module.module}")

    File.mkdir_p!(assembly_dir)

    # create the main code file
    code_file = Path.join(assembly_dir, "#{module.module}.zig")
    code_content = Zigler.Code.generate_main(%{module | zig_file: code_file})

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
