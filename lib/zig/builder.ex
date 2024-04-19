defmodule Zig.Builder do
  @moduledoc """
  Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  build code with the `zig build` command.
  """

  require EEx
  require Logger
  alias Zig.Command

  def staging_directory(module) do
    Path.join(System.tmp_dir(), to_string(module))
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
      link_lib: module.link_lib,
      include_dir: module.include_dir,
      c_src: module.c_src,
      packages: make_packages(module),
      zig_code_path: module.zig_code_path
    }

    assigns |> dbg

    build_file = build_zig(assigns)

    build_zig_path = Path.join(staging_directory, "build.zig")

    File.write!(build_zig_path, build_file)
    Command.fmt(build_zig_path)

    Logger.debug("wrote build.zig to #{build_zig_path}")

    %{module | module_code_path: Path.join(staging_directory, "module.zig")}
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
