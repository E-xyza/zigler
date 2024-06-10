defmodule Zig.Type.Struct do
  alias Zig.Type
  use Type

  defstruct [:name, :packed, :required, :optional, extern: false, mutable: false]

  @type t :: %{
          name: String.t(),
          packed: nil | non_neg_integer(),
          extern: nil | non_neg_integer(),
          required: %{optional(atom) => Type.t()},
          optional: %{optional(atom) => Type.t()},
          mutable: boolean
        }

  def from_json(%{"name" => name, "fields" => fields} = json, module) do
    {required, optional} = Enum.split_with(fields, & &1["required"])
    to_field = fn desc -> {String.to_atom(desc["name"]), Type.from_json(desc["type"], module)} end

    %__MODULE__{
      name: String.trim_leading(name, ".#{module}."),
      packed: Map.get(json, "packed_size"),
      extern: Map.get(json, "extern", false),
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

  #def render_elixir_spec(struct, :param, opts) do
  #  optional = to_fields(struct.optional, :optional, :param, opts)
  #  keyword = to_fields(struct.optional, :untagged, :param, opts)
  #  required = to_fields(struct.required, :untagged, :param, opts)
#
  #  if binary_form = binary_form(struct) do
  #    quote do
  #      unquote(map_spec(optional, required))
  #      | unquote(keyword ++ required)
  #      | unquote(binary_form)
  #    end
  #  else
  #    quote do
  #      unquote(map_spec(optional, required)) | unquote(keyword ++ required)
  #    end
  #  end
  #end
#
  #def render_elixir_spec(struct, :return, opts) do
  #  binary_form = binary_form(struct)
#
  #  case opts.as do
  #    :binary when not is_nil(binary_form) ->
  #      binary_form
#
  #    t when t in ~w(list binary default)a ->
  #      all_fields =
  #        struct.optional
  #        |> Map.merge(struct.required)
  #        |> to_fields(:required, :return, opts)
#
  #      map_spec([], all_fields)
  #  end
  #end
#
  defp map_spec(optional, required) do
    quote context: Elixir do
      %{unquote_splicing(optional ++ required)}
    end
  end

  defp binary_form(%{packed: int}) when is_integer(int) do
    quote context: Elixir do
      <<_::unquote(int * 8)>>
    end
  end

  defp binary_form(_struct), do: nil

  defp to_fields(portion, mode, context, opts) do
    portion
    |> Enum.map(fn
      {k, v} when mode == :optional ->
        {quote do
           optional(unquote(k))
         end, Type.render_elixir_spec(v, context, opts)}

      {k, v} ->
        {k, Type.render_elixir_spec(v, context, opts)}
    end)
    |> Enum.sort()
  end

  def render_zig(%{name: name}), do: "nif.#{name}"

  # for now.  Later, we will need to do more sophisticated checks
  def get_allowed?(_), do: true

  def make_allowed?(struct) do
    struct.required
    |> Map.values()
    |> Kernel.++(Map.values(struct.optional))
    |> Enum.map(&Type.make_allowed?/1)
    |> Enum.all?()
  end

  def can_cleanup?(_), do: false

  def binary_size(%{packed: packed}) when is_integer(packed), do: packed
  def binary_size(%{extern: extern}) when is_integer(extern), do: extern
  def binary_size(_), do: nil

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()
end
