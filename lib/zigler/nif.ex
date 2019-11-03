defmodule Zigler.Nif do
  @moduledoc """
  This datastructure represents structured information about a single nif
  inside of a sigil_Z block.
  """

  @enforce_keys [:name, :arity]
  defstruct @enforce_keys ++ [doc: nil, params: [], retval: nil]

  @type t :: %__MODULE__{
    name:   atom,
    arity:  non_neg_integer,
    doc:    iodata | nil,
    params: [String.t],
    retval: String.t
  }

end
