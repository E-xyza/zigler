defmodule Zig.Type.Function do
  # function gets an access behaviour so that it can be easily used in EEx
  # files.
  @behaviour Access

  defstruct [:name, :arity, :params, :return]
  alias Zig.Type

  @impl true
  defdelegate fetch(function, key), to: Map

  @type t :: %__MODULE__{
          name: atom(),
          arity: non_neg_integer(),
          params: [Type.t()],
          return: Type.t()
        }

  def from_json(%{"name" => name, "params" => params, "return" => return}) do
    params = Enum.map(params, &Type.from_json/1)

    arity =
      case params do
        [:env | rest] -> length(rest)
        _ -> length(params)
      end

    %__MODULE__{
      name: String.to_atom(name),
      arity: arity,
      params: params,
      return: Type.from_json(return)
    }
  end

  def param_marshalling_macros(%{params: params}) do
    list = Enum.map(params, &Type.marshal_param/1)
    if Enum.any?(list), do: list, else: nil
  end

  def return_marshalling_macro(%{return: return}) do
    Type.marshal_return(return)
  end

  def param_error_macros(%{params: params}) do
    list = Enum.map(params, &Type.param_errors/1)
    if Enum.any?(list), do: list, else: nil
  end

  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")

  # eliminates module prefix from
  def scrub_structs(function, module) do
    %{
      function
      | params: Enum.map(function.params, &scrub_struct(&1, module)),
        return: scrub_struct(function.return, module)
    }
  end

  alias Zig.Type.Struct

  def scrub_struct(struct = %Struct{}, module) do
    %{struct | name: String.trim_leading(struct.name, ".#{module}.")}
  end

  def scrub_struct(non_struct, _), do: non_struct
end
