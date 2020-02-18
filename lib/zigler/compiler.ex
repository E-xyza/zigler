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

  alias Zigler.Compiler.ErrorParser
  alias Zigler.Import
  alias Zigler.Zig

  @zig_dir_path Path.expand("../../../zig", __ENV__.file)
  @erl_nif_zig_h Path.join(@zig_dir_path, "include/erl_nif_zig.h")
  @erl_nif_zig Path.join(@zig_dir_path, "beam/erl_nif.zig")

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

  @release_mode %{
    fast:  ["--release-fast"],
    safe:  ["--release-safe"],
    small: ["--release-small"],
    debug: []
  }

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
      compile(compiler, zig_tree)
    end
    cleanup(compiler)

    ###########################################################################
    # MACRO SETPS

    nif_functions = Enum.map(module.nifs, &function_skeleton/1)

    mod_path = module.app
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

  def function_skeleton(nif = %{opts: opts}) do
    if opts[:long] do
      quote context: Elixir do
        unquote(long_main_fn(nif))
        unquote(long_launch_fn(nif))
        unquote(long_fetch_fn(nif))
      end
    else
      basic_fn(nif)
    end
  end

  defp basic_fn(%{name: name, arity: arity}) do
    text = "nif for function #{name}/#{arity} not bound"

    params = if arity == 0 do
      Elixir
    else
      for _ <- 1..arity, do: {:_, [], Elixir}
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {name, [context: Elixir], params},
        [do: {:raise, [context: Elixir, import: Kernel], [text]}]
      ]}
  end

  alias Zigler.Code.LongRunning

  defp long_main_fn(%{name: name, arity: arity}) do
    params = if arity == 0 do
      []
    else
      for idx <- 1..arity, do: {String.to_atom("arg#{idx}"), [], Elixir}
    end

    launcher_call = {LongRunning.launcher(name), [], params}

    block = quote context: Elixir do
      resource = unquote(launcher_call)
      receive do {:done, ^resource} -> :ok end
      unquote(LongRunning.fetcher name)(resource)
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {name, [context: Elixir], params},
        [do: block]
      ]}
  end

  defp long_launch_fn(%{name: name, arity: arity}) do
    text = "nif launcher for function #{name}/#{arity} not bound"

    params = if arity == 0 do
      []
    else
      for _ <- 1..arity, do: {:_, [], Elixir}
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {LongRunning.launcher(name), [context: Elixir], params},
        [do: {:raise, [context: Elixir, import: Kernel], [text]}]
      ]}
  end

  defp long_fetch_fn(%{name: name, arity: arity}) do
    text = "nif fetcher for function #{name}/#{arity} not bound"
    quote context: Elixir do
      def unquote(LongRunning.fetcher name)(_) do
        raise unquote(text)
      end
    end
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
    File.write!(code_file, Zigler.Code.generate_main(module))

    # copy in beam.zig
    File.cp!("zig/beam/beam.zig", Path.join(staging_dir, "beam.zig"))
    # copy in erl_nif.zig
    File.cp!("zig/beam/erl_nif.zig", Path.join(staging_dir, "erl_nif.zig"))
    # copy in erl_nif_zig.h
    File.mkdir_p!(Path.join(staging_dir, "include"))
    File.cp!("zig/include/erl_nif_zig.h", Path.join(staging_dir, "include/erl_nif_zig.h"))

    # assemble the module struct
    %__MODULE__{
      staging_dir: staging_dir,
      code_file:   code_file,
      module_spec: module
    }
  end

  @spec compile(t, Path.t) :: :ok | no_return
  defp compile(compiler, zig_tree) do
    # first move everything into the staging directory.
    Zig.compile(compiler, zig_tree)
    :ok
  end

  @spec cleanup(t) :: :ok | no_return
  defp cleanup(compiler) do
    # in dev and test we keep our code around for debugging purposes.
    # TODO: make this configurable.
    if Mix.env in [:dev, :prod] do
      File.rm_rf!(compiler.staging_dir)
    end
    :ok
  end
end
