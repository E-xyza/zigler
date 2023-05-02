defmodule Zig.Type.Struct do
  alias Zig.Type
  use Type

  defstruct [:name, :packed, :required, :optional, mutable: false]

  @type t :: %{
          name: String.t(),
          packed: nil | non_neg_integer(),
          required: %{optional(atom) => Type.t()},
          optional: %{optional(atom) => Type.t()},
          mutable: boolean
        }

  def from_json(json = %{"name" => name, "fields" => fields}, module) do
    {required, optional} = Enum.split_with(fields, & &1["required"])
    to_field = fn desc -> {String.to_atom(desc["name"]), Type.from_json(desc["type"], module)} end

    %__MODULE__{
      name: String.trim_leading(name, ".#{module}."),
      packed: Map.get(json, "packed_size"),
      required: Map.new(required, to_field),
      optional: Map.new(optional, to_field)
    }
  end

  def marshal_param(%{packed: packed}, _) when rem(packed, 8) != 0 do
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

  def marshal_param(_, _), do: nil

  def to_string(struct), do: "#{mut(struct)}#{struct.name}"

  def to_call(struct), do: "#{mut(struct)}nif.#{struct.name}"

  def spec(struct, opts) do
    fields =
      struct.required
      |> Map.merge(struct.optional)
      |> Enum.map(fn {k, v} -> {k, Type.spec(v, opts)} end)
      |> Enum.sort()

    quote context: Elixir do
      %{unquote_splicing(fields)}
    end
  end

  defp mut(struct), do: if(struct.mutable, do: "*")

  def return_allowed?(struct) do
    struct.required
    |> Map.values()
    |> Kernel.++(Map.values(struct.optional))
    |> Enum.map(&Type.return_allowed?/1)
    |> Enum.all?()
  end
end
