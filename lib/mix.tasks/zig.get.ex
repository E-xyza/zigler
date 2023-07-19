defmodule Mix.Tasks.Zig.Get do
  use Mix.Task

  @shortdoc "Get the zig from online"

  @moduledoc """
  Get zig from online
  """

  def run(_) do
    Application.ensure_all_started([:inets, :crypto, :public_key])

    Zig.Command.fetch!(Zig.version())
  end
end
