defmodule Zig.Type.Manypointer do
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

  def return_allowed?(pointer), do: pointer.has_sentinel? and Type.return_allowed?(pointer.child)
  def missing_size?(_), do: true

  def marshals_param?(_), do: false
  def marshals_return?(_), do: false
  def render_payload_options(type, index, _), do: Type._default_payload_options()
  def render_return(type), do: Type._default_return()

  # only manypointers of [*:0]u8 are allowed to be returned.
  def spec(%{child: ~t(u8), has_sentinel?: true}, :return, opts) do
    case Keyword.fetch!(opts, :type) do
      :list ->
        [Type.spec(~t(u8), :return, opts)]

      type when type in ~w(default binary)a ->
        Type.spec(:binary)
    end
  end

  def spec(%{child: child, has_sentinel?: sentinel}, :param, opts)
      when not sentinel or child == ~t(u8) do
    if binary_form = binary_form(child) do
      quote context: Elixir do
        unquote([Type.spec(child, :param, opts)]) | unquote(binary_form)
      end
    else
      [Type.spec(child, :param, opts)]
    end
  end

  defp binary_form(~t(u8)), do: Type.spec(:binary)

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

  def of(type, opts \\ []) do
    struct(__MODULE__, opts ++ [child: type])
  end
end
