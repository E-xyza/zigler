defmodule Zigler.Parser.ResourceCleanup do
  @enforce_keys [:for]
  defstruct @enforce_keys ++ [:name, :doc]
end
