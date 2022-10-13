defmodule Zig.Parser.Block do

  defstruct [:name, code: []]

  def post_traverse(rest, [{__MODULE__, _block_parts} | rest_args], context, _, _) do
    {rest, [%__MODULE__{} | rest_args], context}
  end
end
