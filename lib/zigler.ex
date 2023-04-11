defmodule :zigler do
  def parse_transform(ast, opts) do
    ast |> IO.inspect
    opts |> IO.inspect
    ast
  end
end
