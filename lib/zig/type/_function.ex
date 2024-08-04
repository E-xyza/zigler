defmodule Zig.Type.Function do
  @moduledoc false

  # functions, as identified by performing semantic analysis on the zig code.

  defstruct [:name, :arity, :params, :return]

  alias Zig.Type

  @type t :: %__MODULE__{
          name: atom(),
          arity: non_neg_integer(),
          params: [Type.t()],
          return: Type.t()
        }

  def from_json(%{"params" => params, "return" => return, "name" => name}, module) do
    params = Enum.map(params, &Type.from_json(&1, module))

    %__MODULE__{
      name: String.to_atom(name),
      arity: length(params),
      params: params,
      return: Type.from_json(return, module)
    }
  end
end
