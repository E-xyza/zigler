defmodule Zig.Type.Struct do
  use Zig.Type

  @derive Inspect
  defstruct [:name, :packed]

  @type t :: %{
          name: String.t(),
          packed: nil | non_neg_integer()
        }

  def from_json(json = %{"name" => name}, module) do
    %__MODULE__{
      name: String.trim_leading(name, ".#{module}."),
      packed: Map.get(json, "packed_size")
    }
  end

  def marshal_param(%{packed: packed}) when rem(packed, 8) != 0 do
    # padding bits
    padding = 8 - rem(packed, 8)

    fn arg, _index ->
      quote do
        if is_bitstring(unquote(arg)) do
          <<0::unquote(padding), unquote(arg)::bitstring>>
          |> :erlang.binary_to_list()
          |> Enum.reverse()
          |> :erlang.list_to_binary()
        else
          unquote(arg)
        end
      end
    end
  end

  def marshal_param(_), do: nil

  def marshal_return(_), do: nil

  def param_errors(_), do: nil

  def to_string(struct), do: struct.name

  def to_call(struct), do: "nif." <> struct.name
end
