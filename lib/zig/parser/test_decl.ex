defmodule Zig.Parser.TestDecl do

  @enforce_keys [:linecol]
  defstruct @enforce_keys ++ [:name, code: []]

  alias Zig.Parser.Block

  def post_traverse(rest, [{__MODULE__, args} | rest_args], context, linecol, _) do
    {rest, [structure(args, linecol) | rest_args], context}
  end

  defp structure([:test, block = %Block{}], linecol) do
    %__MODULE__{linecol: linecol, code: block}
  end

  defp structure([:test, name, block = %Block{}], linecol) when is_binary(name) do
    %__MODULE__{linecol: linecol, code: block, name: name}
  end
end
