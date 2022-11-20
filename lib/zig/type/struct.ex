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

  def marshal_return(_, _), do: nil

  def param_errors(type, _) do
    type_str = to_string(type)

    fn index ->
      [
        {{:nif_struct_missing_field_error, index},
         quote do
           case __STACKTRACE__ do
             [{_m, _f, args, _opts}, {m, f, _, opts} | rest] ->
               # obtain the argument at index
               arg_keys =
                 args
                 |> Enum.at(unquote(index))
                 |> case do
                   map when is_map(map) ->
                     Map.keys(map)

                   list when is_list(list) ->
                     Keyword.keys(list)
                 end

               missing_key = List.first(unquote(Map.keys(type.required)) -- arg_keys)

               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{
                     unquote(index + 1) =>
                       "\n\n     expected argument of type `#{unquote(type_str)}` to have field #{inspect(missing_key)}"
                   }
                 )

               :erlang.raise(:error, :badarg, [{m, f, args, new_opts} | rest])

             stacktrace ->
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end},
        {{:nif_struct_field_error, index},
         quote do
           case __STACKTRACE__ do
             [{_m, _f, args, _opts}, {m, f, _, opts} | rest] ->
               new_opts =
                 Keyword.merge(opts,
                   error_info: %{module: __MODULE__, function: :_format_error},
                   zigler_error: %{
                     unquote(index + 1) =>
                       "\n\n     expected argument of type `#{unquote(type_str)}` but one of its fields has incorrect type"
                   }
                 )

               :erlang.raise(:error, :badarg, [{m, f, args, new_opts} | rest])

             stacktrace ->
               :erlang.raise(:error, :badarg, stacktrace)
           end
         end}
      ]
    end
  end

  def to_string(struct), do: "#{mut(struct)}#{struct.name}"

  def to_call(struct), do: "#{mut(struct)}nif.#{struct.name}"

  defp mut(struct), do: if(struct.mutable, do: "*")

  def return_allowed?(struct) do
    struct.required
    |> Map.values()
    |> Kernel.++(Map.values(struct.optional))
    |> Enum.map(&Type.return_allowed?/1)
    |> Enum.all?()
  end
end
