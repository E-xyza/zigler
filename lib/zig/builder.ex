defmodule Zig.Builder do
  @moduledoc false

  # Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  # build code with the `zig build` command.

  require EEx
  require Logger
  alias Zig.Attributes
  alias Zig.Command

  def staging_directory(module) do
    staging_root =
      case System.get_env("ZIGLER_STAGING_ROOT", "") do
        "" -> System.tmp_dir()
        path -> path
      end

    Path.join(staging_root, to_string(module))
  end

  build_zig_template = Path.join(__DIR__, "templates/build.zig.eex")
  EEx.function_from_file(:defp, :build_zig, build_zig_template, [:assigns])

  def stage(module) do
    staging_directory = staging_directory(module.module)

    unless File.dir?(staging_directory) do
      Logger.debug("creating staging directory #{staging_directory}")

      File.mkdir_p!(staging_directory)
    end

    # TODO: start using the module struct directly.

    assigns = %{
      module: module.module,
      version: module.version,
      beam_dir: Path.join(:code.priv_dir(:zigler), "beam"),
      c: module.c,
      packages: make_packages(module),
      zig_code_path: module.zig_code_path
    }

    assigns =
      case :os.type() do
        {_, :nt} ->
          :zigler
          |> :code.priv_dir()
          |> Path.join("erl_nif_win")
          |> then(&Map.put(assigns, :windows_shim_dir, &1))

        _ ->
          assigns
      end

    build_file = build_zig(assigns)

    attribs_path = Path.join(staging_directory, "attributes.zig")
    File.write!(attribs_path, Enum.map(module.attributes, &Attributes.render_zig/1))

    build_zig_path = Path.join(staging_directory, "build.zig")

    File.write!(build_zig_path, build_file)
    Command.fmt(build_zig_path)

    Logger.debug("wrote build.zig to #{build_zig_path}")

    %{module | module_code_path: Path.join(staging_directory, "module.zig")}
  rescue
    e in File.Error ->
      new_action = "#{e.action}, consider setting ZIGLER_STAGING_ROOT environment variable\n"
      reraise %{e | action: new_action}, __STACKTRACE__
  end

  defp make_packages(opts) do
    List.wrap(
      if packages = opts.packages do
        Enum.map(packages, fn {name, {path, deps}} ->
          {name, Path.absname(path), deps}
        end)
      end
    )
  end
end
