defmodule Mix.Tasks.Zig.Version do
  @moduledoc false
  use Mix.Task

  @shortdoc "Relays the zig version being used"

  alias Zig.Command

  def run(_) do
    zig_cmd = Command.executable_path()

    case System.cmd(zig_cmd, ["version"], []) do
      {version, 0} ->
        IO.puts(version)

      error ->
        Mix.raise("errored with #{inspect(error)}")
    end
  end
end
