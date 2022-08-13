defmodule Zig.Type.Function do
  defstruct [:name, :arity, :params, :return]
  alias Zig.Type

  @type t :: %__MODULE__{
    name: atom(),
    arity: non_neg_integer(),
    params: [Type.t],
    return: Type.t
  }
end
