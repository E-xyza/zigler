defmodule Zig.Type.Array do
  @moduledoc false

  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  use Type

  import Type, only: :macros

  defstruct [:child, :len, :repr, has_sentinel?: false, mutable: false]

  @type t :: %__MODULE__{
          child: Type.t(),
          repr: String.t(),
          len: non_neg_integer,
          mutable: boolean,
          has_sentinel?: boolean
        }

  def from_json(
        %{"child" => child, "len" => len, "has_sentinel" => has_sentinel?, "repr" => repr},
        module
      ) do
    %__MODULE__{
      child: Type.from_json(child, module),
      len: len,
      has_sentinel?: has_sentinel?,
      repr: repr
    }
  end

  @impl true
  def get_allowed?(array), do: Type.get_allowed?(array.child)
  @impl true
  def make_allowed?(array), do: Type.make_allowed?(array.child)
  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(%{sentinel: true} = array) do
    case Type.binary_size(array.child) do
      size when is_integer(size) -> size * array.len
      {:indirect, size} -> size * array.len
      _ -> nil
    end
  end

  @impl true
  def binary_size(array) do
    case Type.binary_size(array.child) do
      size when is_integer(size) -> array_binary_size(array, size)
      {:indirect, size} -> array_binary_size(array, size)
      _ -> nil
    end
  end

  defp array_binary_size(array, size) do
    if array.has_sentinel?, do: {:var, size}, else: size * array.len
  end

  @impl true
  def render_zig(%{mutable: true} = array) do
    "*" <> render_zig(%{array | mutable: false})
  end

  def render_zig(array), do: array.repr

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

  @impl true
  def render_elixir_spec(type, %Parameter{} = param) do
    # u8 defaults to binary
    if binary_typespec = Type.binary_typespec(type) do
      quote context: Elixir do
        unquote([Type.render_elixir_spec(type.child, param)]) | unquote(binary_typespec)
      end
    else
      [Type.render_elixir_spec(type.child, param)]
    end
  end

  def render_elixir_spec(type, %Return{as: as}), do: render_elixir_spec(type, as)

  def render_elixir_spec(type, {:list, subspec}) do
    [Type.render_elixir_spec(type.child, subspec)]
  end

  def render_elixir_spec(type, :list) do
    [Type.render_elixir_spec(type.child, :default)]
  end

  def render_elixir_spec(type, :binary) do
    Type.binary_typespec(type) || raise "Binary form not possible for type #{inspect(type)}"
  end

  def render_elixir_spec(type, :default) do
    case type.child do
      ~t(u8) -> Type.binary_typespec(type)
      _ -> [Type.render_elixir_spec(type.child, :default)]
    end
  end

  def of(type, len, opts \\ []) do
    struct(__MODULE__, opts ++ [child: type, len: len])
  end
end
