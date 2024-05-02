defmodule Zig.Parameter do 
  @enforce_keys [:type]
  defstruct @enforce_keys

  alias Zig.Type

  @type t :: %__MODULE__{
    type: Type.t
  }

  def new(type, _options) do
    struct!(__MODULE__, type: type)
  end
end