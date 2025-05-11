defmodule Zig.Type.Slice do
  @moduledoc false

  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  use Type

  @enforce_keys ~w[child repr const]a
  defstruct @enforce_keys ++ [has_sentinel?: false]

  import Type, only: :macros

  @type t :: %__MODULE__{
          child: Type.t(),
          repr: String.t(),
          has_sentinel?: boolean
        }

  def from_json(
        %{"child" => child, "has_sentinel" => has_sentinel?, "repr" => repr, "is_const" => const},
        module
      ) do
    %__MODULE__{
      child: Type.from_json(child, module),
      has_sentinel?: has_sentinel?,
      const: const,
      repr: repr
    }
  end

  # TYPE SPEC STUFF

  @impl true
  def render_elixir_spec(type, %Return{as: as}) do
    render_elixir_spec(type, as)
  end

  def render_elixir_spec(type, %Parameter{} = params) do
    if typespec = Type.binary_typespec(type) do
      quote context: Elixir do
        [unquote(Type.render_elixir_spec(type.child, params))] | unquote(typespec)
      end
    else
      quote context: Elixir do
        [unquote(Type.render_elixir_spec(type.child, params))]
      end
    end
  end

  def render_elixir_spec(%{child: child}, {:list, child_spec}) do
    [Type.render_elixir_spec(child, child_spec)]
  end

  def render_elixir_spec(%{child: child}, :list) do
    [Type.render_elixir_spec(child, :default)]
  end

  def render_elixir_spec(spec, :binary) do
    Type.binary_typespec(spec)
  end

  def render_elixir_spec(%{child: ~t(u8)}, :default) do
    quote do
      binary()
    end
  end

  def render_elixir_spec(%{child: child}, :default) do
    [Type.render_elixir_spec(child, :default)]
  end

  @impl true
  def render_zig(slice), do: slice.repr

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  # ETC

  @impl true
  def get_allowed?(slice), do: Type.get_allowed?(slice.child)
  @impl true
  def make_allowed?(slice), do: Type.make_allowed?(slice.child)
  @impl true
  def in_out_allowed?(slice), do: get_allowed?(slice) and make_allowed?(slice)

  @impl true
  def binary_size(slice) do
    case Type.binary_size(slice.child) do
      size when is_integer(size) -> {:var, size}
      {:indirect, size} when is_integer(size) -> {:var, size}
      _ -> nil
    end
  end

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()
  @impl true
  def payload_options(_, _), do: Type._default_payload_options()
  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)

  def of(child, opts \\ []), do: struct(__MODULE__, [child: child] ++ opts)
end
