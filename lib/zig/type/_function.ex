defmodule Zig.Type.Function do
  @moduledoc false

  # functions, as identified by performing semantic analysis on the zig code.

  defstruct [:name, :arity, :params, :return]

  alias Zig.Type
  alias Zig.Type.Integer
  alias Zig.Type.Manypointer

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

  @spec raw(Function.t()) :: nil | :term | :erl_nif_term

  @u32 %Integer{signedness: :signed, bits: 32}
  @term ~w[term erl_nif_term]a

  def raw(%{arity: 3, params: [:env, @u32, %Manypointer{child: child, has_sentinel?: false}]})
      when child in @term,
      do: child

  def raw(_), do: nil
end
