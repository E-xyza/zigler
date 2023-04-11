defmodule :zigler do
  def parse_transform(ast, _opts) do
    {code, pos} =
      case Enum.find(ast, &match?({:attribute, _, :zig, _}, &1)) do
        nil -> raise "No zig code found"
        {:attribute, pos, :zig, code} -> {code, pos}
      end

    opts =
      case Enum.find(ast, &match?({:attribute, _, :zig_opts, _}, &1)) do
        nil -> raise "No zig opts found"
        {:attribute, _, :zig_opts, opts} -> opts
      end

    {code, pos, opts} |> IO.inspect()

    ast
  end
end
