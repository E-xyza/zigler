defmodule Zig.Type.Function do
  @moduledoc """
  module representing the zig type, as identified by performing semantic
  analysis on the zig code.
  """

  defstruct [:name, :arity, :params, :return]

  alias Zig.Manifest
  alias Zig.Type

  @type t :: %__MODULE__{
          name: atom(),
          arity: non_neg_integer(),
          params: [Type.t()],
          return: Type.t()
        }

  def from_json(%{"params" => params, "return" => return, "name" => name}, module) do
    params = Enum.map(params, &Type.from_json(&1, module))

    arity =
      case params do
        [:env | rest] -> length(rest)
        _ -> length(params)
      end

    function = %__MODULE__{
      name: String.to_atom(name),
      arity: arity,
      params: params,
      return: Type.from_json(return, module)
    }
  end
end
