defmodule Zig.Macro do
  def inspect(macro) do
    macro
    |> Macro.to_string()
    |> Code.format_string!()
    |> IO.puts()

    macro
  end
end
