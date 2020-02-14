defmodule Zigler.Parser.Resource do
  @enforce_keys [:name]
  defstruct @enforce_keys ++ [:doc]
end
