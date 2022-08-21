defmodule Zig.Builder do
  @moduledoc """
  Code for interfacing with `std.build.Builder`, the interface for programmatically invoking
  build code with the `zig build` command.
  """

  require EEx
  require Logger

  build_zig_template = Path.join(__DIR__, "templates/build.zig.eex")
  EEx.function_from_file(:defp, :build_zig, build_zig_template, [:assigns])

  def build(module, opts) do
    assigns = %{
      module: module,
      # TODO: fix this!
      version: Version.parse!("0.0.0"),
      code_file: ".#{module}.zig",
      nif_path: Path.join(opts[:from], ".#{module}.zig"),
      beam_dir: Path.join(:code.priv_dir(:zigler), "beam")
    }

    build_file = build_zig(assigns)

    build_zig_path = Path.join(opts[:to], "build.zig")

    File.write!(build_zig_path, build_file)

    Logger.debug("wrote build.zig to #{build_zig_path}")
  end
end
