defmodule Zig.Macro do
  @moduledoc false

  # macro helper tools

  def inspect(macro, opts) do
    if opts[:dump] do
      macro
      |> Macro.to_string()
      |> IO.puts()

      macro
    else
      macro
    end
  end
end
