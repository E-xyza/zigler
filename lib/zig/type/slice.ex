defmodule Zig.Type.Slice do
  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  use Type

  import Type, only: :macros

  defstruct [:child, :repr, has_sentinel?: false]

  @type t :: %__MODULE__{
          child: Type.t(),
          repr: String.t(),
          has_sentinel?: boolean
        }

  def from_json(
        %{"child" => child, "has_sentinel" => has_sentinel?, "repr" => repr},
        module
      ) do
    %__MODULE__{
      child: Type.from_json(child, module),
      has_sentinel?: has_sentinel?,
      repr: repr
    }
  end

  # TYPE SPEC STUFF

  def render_elixir_spec(type, %Return{as: as}) do
    render_elixir_spec(type, as)
  end

  def render_elixir_spec(%{child: child}, %Parameter{} = params) do
    quote context: Elixir do
      [unquote(Type.render_elixir_spec(child, params))] | unquote(binary_form(child))
    end
  end

  def render_elixir_spec(%{child: ~t(u8)}, :default) do
    binary_form(~t(u8))
  end

  def render_elixir_spec(%{child: child}, :binary) do
    binary_form(child)
  end

  def render_elixir_spec(%{child: child}, {:list, child_spec}) do
    [Type.render_elixir_spec(child, child_spec)]
  end

  def render_elixir_spec(%{child: child}, _) do
    [Type.render_elixir_spec(child, :default)]
  end

  defp binary_form(~t(u8)) do
    quote do
      binary()
    end
  end

  defp binary_form(%Type.Integer{bits: bits}) do
    quote context: Elixir do
      <<_::_*unquote(Type.Integer._next_power_of_two_ceil(bits))>>
    end
  end

  defp binary_form(%Type.Float{bits: bits}) do
    quote context: Elixir do
      <<_::_*unquote(bits)>>
    end
  end

  defp binary_form(%Type.Struct{packed: size}) when is_integer(size) do
    quote context: Elixir do
      <<_::_*unquote(size * 8)>>
    end
  end

  defp binary_form(_), do: nil

  # ETC

  def get_allowed?(slice), do: Type.get_allowed?(slice.child)
  def make_allowed?(slice), do: Type.make_allowed?(slice.child)
  def can_cleanup?(_), do: true

  def binary_size(slice) do
    case Type.binary_size(slice.child) do
      size when is_integer(size) -> {:var, size}
      _ -> nil
    end
  end

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()

  def of(child, opts \\ []), do: struct(__MODULE__, [child: child] ++ opts)
end
