defmodule Zig.Macro do
  @moduledoc """
  """

  def inspect(macro, opts) do
    if opts.dump do
      macro
      |> Macro.to_string()
      |> IO.puts()

      macro
    else
      macro
    end
  end
end
