defmodule Zig.Type.Array do
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

  def get_allowed?(array), do: Type.get_allowed?(array.child)
  def make_allowed?(array), do: Type.make_allowed?(array.child)
  def can_cleanup?(_), do: false

  def binary_size(%{sentinel: true} = array) do
    case Type.binary_size(array.child) do
      size when is_integer(size) -> size * array.len
      {:indirect, size} -> size * array.len
      _ -> nil
    end
  end

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

  def render_zig(%{mutable: true} = array) do
    "*" <> render_zig(%{array | mutable: false})
  end

  def render_zig(array), do: array.repr

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()

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
