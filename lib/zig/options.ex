defmodule Zig.Options do
  @moduledoc """
  creates `options.zig` file which is mapped to `@import("zigler_options")` in
  `beam.zig`.  This is then exposed as `@import("beam").options` in your code.
  """

  require EEx
  require Logger

  options_zig_template = Path.join(__DIR__, "templates/options.zig.eex")
  EEx.function_from_file(:defp, :options_zig, options_zig_template, [:assigns])

  def build(_module, opts) do
    build_zig_path = Path.join(opts[:to], "options.zig")

    File.write!(build_zig_path, options_zig(opts))

    Logger.debug("wrote options.zig to #{build_zig_path}")
  end
end
