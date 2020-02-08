defmodule Zigler.Module do
  @moduledoc """
  this struct represents all information a zigler module bound to a
  nif should have.
  """

  @enforce_keys [:file]

  defstruct @enforce_keys ++ [nifs: []]

  @type t :: %__MODULE__{
    nifs: [Zigler.Parser.Function.t],
    file: Path.t
  }
end
