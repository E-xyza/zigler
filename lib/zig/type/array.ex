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
      _ -> nil
    end
  end

  def binary_size(array) do
    case Type.binary_size(array.child) do
      size when is_integer(size) -> size * array.len
      _ -> nil
    end
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
    if binary_form = binary_form(type) do
      quote context: Elixir do
        unquote([Type.render_elixir_spec(type.child, param)]) | unquote(binary_form)
      end
    else
      [Type.render_elixir_spec(type.child, param)]
    end
  end

  def render_elixir_spec(type, %Return{as: as}), do: render_elixir_spec(type, as)

  def render_elixir_spec(type, {:list, subspec}) do
    [Type.render_elixir_spec(type.child, subspec)]
  end

  def render_elixir_spec(%{child: ~t(u8)} = type, as) do
    # u8 defaults to binary
    case as do
      :list ->
        [Type.render_elixir_spec(~t(u8), :default)]

      t when t in ~w(default binary)a ->
        binary_form(type)
    end
  end

  def render_elixir_spec(type, as) do
    # other types default to list
    case as do
      :binary ->
        binary_form(type)

      other when other in ~w(default list)a ->
        [Type.render_elixir_spec(type.child, :default)]
    end
  end

  defp binary_form(%{child: ~t(u8), has_sentinel?: true}) do
    quote do
      binary
    end
  end

  defp binary_form(type) do
    case Type.binary_size(type.child) do
      child_size when is_integer(child_size) and type.has_sentinel? ->
        quote do
          <<_::_*unquote(child_size)>>
        end

      child_size when is_integer(child_size) ->
        quote do
          <<_::unquote(child_size * type.len)>>
        end

      _ ->
        nil
    end
  end

  def of(type, len, opts \\ []) do
    struct(__MODULE__, opts ++ [child: type, len: len])
  end
end
