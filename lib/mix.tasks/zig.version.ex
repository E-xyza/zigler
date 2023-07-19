defmodule Mix.Tasks.Zig.Version do
  use Mix.Task

  @shortdoc "Get the zig version"

  @moduledoc """
  Get the zig version
  """

  def run(_) do
    IO.puts(Zig.version())
  end
end
