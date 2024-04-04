defmodule Zig.Builder do
  @moduledoc """
  Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  build code with the `zig build` command.
  """

  require EEx
  require Logger
  alias Zig.Command

  build_zig_template = Path.join(__DIR__, "templates/build.zig.eex")
  EEx.function_from_file(:defp, :build_zig, build_zig_template, [:assigns])

  def build(module, opts) do
    assigns = %{
      module: module,
      # TODO: fix this!
      version: Version.parse!("0.0.0"),
      code_file: ".#{module}.zig",
      nif_path: Path.join(opts[:from], ".#{module}.zig"),
      beam_dir: Path.join(:code.priv_dir(:zigler), "beam"),
      link_lib: opts[:link_lib],
      stage1: opts[:stage1],
      include_dir: opts[:include_dir],
      c_src: opts[:c_src],
      packages: make_packages(opts)
    }

    build_file = build_zig(assigns)

    build_zig_path = Path.join(opts[:to], "build.zig")

    File.write!(build_zig_path, build_file)
    Command.fmt(build_zig_path, opts)

    Logger.debug("wrote build.zig to #{build_zig_path}")
  end

  defp make_packages(opts) do
    List.wrap(
      if packages = opts[:packages] do
        Enum.map(packages, fn {name, {path, deps}} ->
          {name, Path.absname(path), deps}
        end)
      end
    )
  end
end
