use Protoss

defprotocol Zig.Builder do
  @moduledoc false

  # Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  # build code with the `zig build` command.

  @spec render_build(t) :: iodata()
  def render_build(assigns, opts)
after
  require EEx
  require Logger
  alias Zig.Attributes
  alias Zig.Command

  defmacro __using__(opts) do
    template = Keyword.fetch!(opts, :template)

    quote do
      defdelegate fetch(struct, key), to: Map

      require EEx
      render_template = Path.join(__DIR__, unquote(template))
      EEx.function_from_file(:def, :render_build, render_template, [:assigns, :opts])
      defoverridable render_build: 2
    end
  end

  def render_build(assigns), do: render_build(assigns, [])

  def staging_directory(module) do
    staging_root =
      case System.get_env("ZIGLER_STAGING_ROOT", "") do
        "" -> Zig._tmp_dir()
        path -> path
      end

    Path.join(staging_root, to_string(module))
  end

  # this is required because Elixir version < 1.16 doesn't support Path.relative_to/3
  def staging_directory(module, from) do
    staging_dir = staging_directory(module)

    # On Windows, always copy dependencies into the staging directory because
    # Zig's build.zig.zon requires relative paths, and Windows path issues
    # (different drives, path length limits) can make relative paths problematic.
    if :os.type() == {:win32, :nt} do
      {:copy, staging_dir, from}
    else
      normalized_staging = norm(staging_dir)
      normalized_from = norm(from)

      case {normalized_staging, normalized_from} do
        {<<drive>> <> ":/" <> mod_rest, <<drive>> <> ":/" <> from_rest} ->
          from_rest
          |> String.split("/")
          |> force_relative(String.split(mod_rest, "/"))

        {"/" <> mod_rest, "/" <> from_rest} ->
          from_rest
          |> String.split("/")
          |> force_relative(String.split(mod_rest, "/"))

        {dir_mod, _} ->
          Path.relative_to(from, dir_mod)
      end
    end
  end

  if {:win32, :nt} == :os.type() do
    defp norm(path) do
      path
      |> String.replace("\\", "/")
      |> case do
        <<a, ?:, rest::binary>> when a in ?A..?Z ->
          <<a + 32, ?:, rest::binary>>

        other ->
          other
      end
    end
  else
    defp norm(path), do: path
  end

  defp force_relative([same | rest_left], [same | rest_right]),
    do: force_relative(rest_left, rest_right)

  defp force_relative([], []), do: "."

  defp force_relative(left, others) do
    others
    |> length()
    |> then(&List.duplicate("..", &1))
    |> Path.join()
    |> Path.join(Path.join(left))
  end

  EEx.function_from_file(
    :def,
    :build_zig_zon,
    Path.join(__DIR__, "templates/build.zig.zon.eex"),
    [:assigns]
  )

  def beam_file(path) do
    :zigler
    |> :code.priv_dir()
    |> Path.join("beam/#{path}")
  end

  def stage(module = %{precompiled: nil}) do
    staging_directory = staging_directory(module.module)

    unless File.dir?(staging_directory) do
      Logger.debug("creating staging directory #{staging_directory}")

      # Verify the staging root exists before trying to create subdirectories
      # The staging root should already exist - we only create the module-specific subdirectory
      staging_root =
        case System.get_env("ZIGLER_STAGING_ROOT", "") do
          "" -> Zig._tmp_dir()
          path -> path
        end

      unless File.dir?(staging_root) do
        raise File.Error,
          reason: :enoent,
          action: "make directory (with -p)",
          path: staging_directory
      end

      File.mkdir!(staging_directory)
    end

    libc_txt = build_libc_file(staging_directory)

    # TODO: move to Attributes module.
    attribs_path = Path.join(staging_directory, "attributes.zig")
    File.write!(attribs_path, Enum.map(module.attributes, &Attributes.render_zig/1))

    build_zig_path = Path.join(staging_directory, "build.zig")
    build_zig_zon_path = Path.join(staging_directory, "build.zig.zon")

    if dir = module.build_files_dir do
      # Process dependencies even when using build_files_dir
      process_dependencies(module, staging_directory)

      dir
      |> Zig._normalize_path(Path.dirname(module.file))
      |> Path.join("build.zig")
      |> File.cp!(build_zig_path)

      dir
      |> Zig._normalize_path(Path.dirname(module.file))
      |> Path.join("build.zig.zon")
      |> File.cp!(build_zig_zon_path)
    else
      # Process dependencies - copy them if needed on Windows
      processed_dependencies = process_dependencies(module, staging_directory)

      File.write!(build_zig_path, render_build(%{module | libc_txt: libc_txt}))
      Command.fmt(build_zig_path)

      File.write!(
        build_zig_zon_path,
        build_zig_zon(%{module | dependencies: processed_dependencies})
      )
    end

    Logger.debug("wrote build.zig to #{build_zig_path}")

    %{module | module_code_path: Path.join(staging_directory, "module.zig")}
  rescue
    e in File.Error ->
      new_action = "#{e.action}, consider setting ZIGLER_STAGING_ROOT environment variable\n"
      reraise %{e | action: new_action}, __STACKTRACE__
  end

  # if precompiled file is specified, do nothing.
  def stage(module), do: module

  defp process_dependencies(module, staging_directory) do
    Enum.map(module.dependencies, fn {name, path} ->
      case staging_directory(module.module, path) do
        {:copy, _staging_dir, from} ->
          # Need to copy the dependency into the staging directory
          deps_dir = Path.join(staging_directory, "deps")
          File.mkdir_p!(deps_dir)

          dep_name = Path.basename(from)
          dest = Path.join(deps_dir, dep_name)

          # Copy the entire dependency directory
          Logger.debug("copying dependency #{from} to #{dest}")
          File.cp_r!(from, dest)

          # Return the dependency with the new relative path
          {name, "./deps/#{dep_name}"}

        relative_path ->
          # Normal case - use the relative path as-is
          {name, relative_path}
      end
    end)
  end

  defp build_libc_file(staging_directory) do
    # build a libc file for windows-msvc target
    if match?({_, :windows, :msvc, _}, Application.get_env(:zigler, :precompiling)) do
      staging_directory
      |> Path.join("libc.txt")
      |> File.write!(libc())

      "libc.txt"
    end
  end

  defp libc do
    Regex.replace(
      ~r/\$\{([A-Z0-9_]+)\}/,
      """
      # The directory that contains `stdlib.h` (UCRT headers from the Windows SDK)
      include_dir=${WINSDK_ROOT}/Include/${WINSDK_VER}/ucrt

      # The system-specific include directory (MSVC headers; contains vcruntime.h)
      sys_include_dir=${MSVC_ROOT}/include

      # For Windows, point this to the UCRT library directory in the SDK
      # (Zig uses this field for the CRT libraries on Windows)
      crt_dir=${WINSDK_ROOT}/Lib/${WINSDK_VER}/ucrt/x64

      # MSVC libraries (contains vcruntime.lib, etc.)
      msvc_lib_dir=${MSVC_ROOT}/lib/x64

      # Windows SDK "um" libraries (contains kernel32.lib, user32.lib, etc.)
      kernel32_lib_dir=${WINSDK_ROOT}/Lib/${WINSDK_VER}/um/x64

      # Not used on Windows
      gcc_dir=
      """,
      fn _, var -> System.fetch_env!(var) end
    )
  end
end
