defmodule Zigler.Compiler do

  @moduledoc false

  @enforce_keys [:staging_dir, :code_file, :module_spec]

  # contains critical information for the compilation.
  defstruct @enforce_keys

  @type t :: %__MODULE__{
    staging_dir: Path.t,
    code_file:   Path.t,
    module_spec: Zigler.Module.t
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

  def function_skeleton(nif = %Nif{opts: opts}) do
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

  @staging_root Application.get_env(:zigler, :staging_root, "/tmp/zigler_compiler")

  @spec precompile(Zigler.Module.t) :: t | no_return
  def precompile(module) do
    # build the staging directory.
    staging_dir = Path.join([@staging_root, Atom.to_string(Mix.env()), "#{module.module}"])
    File.mkdir_p(staging_dir)

    # define the code file and build it.
    code_file = Path.join(staging_dir, "#{module.module}.zig")
    File.write!(code_file, Zigler.Code.generate_main(%{module | zig_file: code_file}))

    # copy in beam.zig
    File.cp!("zig/beam/beam.zig", Path.join(staging_dir, "beam.zig"))
    # copy in erl_nif.zig
    File.cp!("zig/beam/erl_nif.zig", Path.join(staging_dir, "erl_nif.zig"))
    # copy in erl_nif_zig.h
    File.mkdir_p!(Path.join(staging_dir, "include"))
    File.cp!("zig/include/erl_nif_zig.h", Path.join(staging_dir, "include/erl_nif_zig.h"))

    # copy imports into the relevant directory
    transfer_imports_for(code_file, Path.dirname(module.file), staging_dir)

    # copy includes into the relevant directory
    transfer_includes_for(Path.dirname(module.file), staging_dir)

    # assemble the module struct
    %__MODULE__{
      staging_dir: staging_dir,
      code_file:   code_file,
      module_spec: module
    }
  end

  defp transfer_imports_for(code_file, src_dir, staging_dir) do
    transfer_imports_for(code_file, src_dir, staging_dir, [])
  end
  defp transfer_imports_for(code_file, src_dir, staging_dir, transferred_files) do

    # mechanism for identifying imported files recursively and moving them into
    # the correct relative directory within the staging zone.

    imports = (code_file
    |> File.read!
    |> Zigler.Parser.Imports.parse
    |> Enum.map(&Path.join(src_dir, &1))
    |> Enum.filter(&(Path.extname(&1) == ".zig"))
    |> Enum.reject(&(Path.basename(&1) in ["beam.zig", "erl_nif.zig"]))
    |> Enum.uniq)
    -- transferred_files

    Enum.each(imports, fn path ->
      rebasename = Path.relative_to(path, src_dir)
      rebasedir = Path.dirname(rebasename)
      stagingfile = Path.join(staging_dir, rebasename)
      stagingfile_dir = Path.dirname(stagingfile)

      File.mkdir_p!(stagingfile_dir)
      # perform transitive imports
      transfer_imports_for(
        path,
        Path.join(src_dir, rebasedir),
        stagingfile_dir,
        Enum.map(imports, &Path.relative_to(&1, rebasedir)))

      File.cp!(path, stagingfile)
    end)
  end

  defp transfer_includes_for(src_dir, staging_dir) do
    staging_include = Path.join(staging_dir, "include")
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
      File.rm_rf!(compiler.staging_dir)
    end
    :ok
  end
end
