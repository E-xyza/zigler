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

    mod_path = module.otp_app
    |> Zigler.nif_dir
    |> Path.join(Zigler.nif_name(module, false))

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
          unquote(mod_path)
          |> String.to_charlist()
          |> :erlang.load_nif(0)
          |> case do
            :ok -> :ok
            {:error, any} ->
              Logger.error("problem loading module #{inspect any}")
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
    assembly = Assembler.parse_code(module.code,
      parent_dir: Path.dirname(module.file),
      target_dir: assembly_dir,
      pub: true,
      context: [])

    Assembler.assemble_kernel!(assembly_dir)
    Assembler.assemble_assets!(assembly)

    %__MODULE__{
      assembly_dir: assembly_dir,
      assembly:     assembly,
      code_file:    code_file,
      module_spec:  module,
    }
  end

  defp transfer_imports_for(compiler, transferred_files \\ []) do

    # mechanism for identifying imported files recursively and moving them into
    # the correct relative directory within the staging zone.

    imports = (compiler.code_file
    |> File.read!
    |> Zigler.Parser.Imports.parse
    |> Keyword.values
    |> Enum.map(&Path.join(compiler.code_dir, &1))
    |> Enum.filter(&(Path.extname(&1) == ".zig"))
    |> Enum.reject(&(Path.basename(&1) in ["beam.zig", "erl_nif.zig"]))
    |> Enum.uniq)
    -- transferred_files

    Enum.each(imports, fn path ->
      rebasename = Path.relative_to(path, compiler.code_dir)
      rebasedir = Path.dirname(rebasename)
      stagingfile = Path.join(compiler.assembly_dir, rebasename)
      stagingfile_dir = Path.dirname(stagingfile)

      File.mkdir_p!(stagingfile_dir)

      transfer_imports_for(
        struct(compiler,
          code_file: path,
          code_dir:  Path.join(compiler.code_dir, rebasedir),
          assembly_dir: stagingfile_dir))

      File.cp!(path, stagingfile)
    end)
  end

  defp transfer_includes_for(src_dir, assembly_dir) do
    staging_include = Path.join(assembly_dir, "include")
    File.mkdir_p!(staging_include)

    src_include = Path.join(src_dir, "include")
    if File.dir?(src_include) do
      File.ls!(src_include)
      for file <- File.ls!(src_include) do
        src_include
        |> Path.join(file)
        |> File.cp_r!(Path.join(staging_include, file))
      end
    end
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
