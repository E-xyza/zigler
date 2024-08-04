defmodule Zig.Type.Struct do
  @moduledoc false

  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  use Type

  defstruct [:name, :packed, :required, :optional, :extern, mutable: false]

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
      extern: Map.get(json, "extern_size"),
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

  @impl true
  def render_elixir_spec(struct, %Return{as: as}) do
    render_elixir_spec(struct, as)
  end

  def render_elixir_spec(struct, %Parameter{} = params) do
    optional = to_fields(struct.optional, :optional, params)
    keyword = to_fields(struct.optional, :untagged, params)
    required = to_fields(struct.required, :untagged, params)

    map_typespec = map_spec(optional, required)
    keyword_typespec = keyword ++ required

    if binary_typespec = Type.binary_typespec(struct) do
      quote context: Elixir do
        unquote(map_typespec) | unquote(keyword_typespec) | unquote(binary_typespec)
      end
    else
      quote do
        unquote(map_typespec) | unquote(keyword_typespec)
      end
    end
  end

  def render_elixir_spec(struct, :binary), do: Type.binary_typespec(struct)

  def render_elixir_spec(%{packed: packed} = struct, :default) when is_integer(packed) do
    Type.binary_typespec(struct)
  end

  # default map form.  Handles `:default`, `:map`, and `{:map, ...}` specs 
  def render_elixir_spec(struct, context) do
    all_fields =
      struct.optional
      |> Map.merge(struct.required)
      |> to_fields(:required, context)

    map_spec([], all_fields)
  end

  defp map_spec(optional, required) do
    quote context: Elixir do
      %{unquote_splicing(optional ++ required)}
    end
  end

  defp to_fields(portion, mode, opts) do
    portion
    |> Enum.map(fn
      {k, v} when mode == :optional ->
        key_opts = key_opts(opts, k)

        {quote do
           optional(unquote(k))
         end, Type.render_elixir_spec(v, key_opts)}

      {k, v} ->
        key_opts = key_opts(opts, k)
        {k, Type.render_elixir_spec(v, key_opts)}
    end)
    |> Enum.sort()
  end

  defp key_opts({:map, keyword}, key) do
    Keyword.get(keyword, key, :default)
  end

  defp key_opts(%Parameter{} = parameter, _), do: parameter

  defp key_opts(_, _), do: :default

  @impl true
  def render_zig(%{name: name}), do: "nif.#{name}"

  # for now.  Later, we will need to do more sophisticated checks
  @impl true
  def get_allowed?(struct) do
    all_values(struct, &Type.get_allowed?/1)
  end

  @impl true
  def make_allowed?(struct) do
    all_values(struct, &Type.make_allowed?/1)
  end

  @impl true
  def in_out_allowed?(_), do: false

  defp all_values(struct, fun) do
    struct.required
    |> Map.values()
    |> Kernel.++(Map.values(struct.optional))
    |> Enum.map(fun)
    |> Enum.all?()
  end

  @impl true
  def binary_size(%{packed: packed}) when is_integer(packed), do: div(packed, 8)
  def binary_size(%{extern: extern}) when is_integer(extern), do: div(extern, 8)
  def binary_size(_), do: nil

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def payload_options(_, _), do: Type._default_payload_options()
  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)
end
