defmodule Zig.Type.Array do
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

  def to_string(array = %{mutable: true}), do: "*" <> to_string(%{array | mutable: false})
  def to_string(%{has_sentinel?: true, repr: repr}), do: repr
  def to_string(array), do: "[#{array.len}]#{Kernel.to_string(array.child)}"

  def to_call(array = %{mutable: true}), do: "*" <> to_call(%{array | mutable: false})
  def to_call(%{has_sentinel?: true, repr: repr}), do: repr
  def to_call(array), do: "[#{array.len}]#{Type.to_call(array.child)}"

  def return_allowed?(array), do: Type.return_allowed?(array.child)

  def spec(type = %{child: ~t(u8)}, :return, opts) do
    # u8 defaults to binary
    case Keyword.fetch!(opts, :type) do
      :charlist ->
        [Type.spec(~t(u8), :return, opts)]

      t when t in ~w(default binary)a ->
        binary_form(~t(u8), known_length(type))
    end
  end

  def spec(type = %{child: child}, :return, opts) do
    # other types defaults to binary
    binary_form = binary_form(child, known_length(type))

    case Keyword.fetch!(opts, :type) do
      :binary when not is_nil(binary_form) ->
        binary_form

      other when other in ~w(default binary charlist)a ->
        [Type.spec(child, :return, opts)]
    end
  end

  def spec(type = %{child: child}, :params, opts) do
    # u8 defaults to binary
    if binary_form = binary_form(child, known_length(type)) do
      quote context: Elixir do
        unquote([Type.spec(child, :params, opts)]) | unquote(binary_form)
      end
    else
      [Type.spec(child, :params, opts)]
    end
  end

  defp known_length(%{len: length, has_sentinel?: sentinel?}) do
    unless sentinel?, do: length
  end

  defp binary_form(~t(u8), nil), do: Type.spec(:binary)

  defp binary_form(%Type.Integer{bits: bits}, length) do
    if length do
      quote context: Elixir do
        <<_::unquote(Type.Integer._next_power_of_two_ceil(bits) * length)>>
      end
    else
      quote context: Elixir do
        <<_::_*unquote(Type.Integer._next_power_of_two_ceil(bits))>>
      end
    end
  end

  defp binary_form(%Type.Float{bits: bits}, length) do
    if length do
      quote context: Elixir do
        <<_::unquote(bits * length)>>
      end
    else
      quote context: Elixir do
        <<_::_*unquote(bits)>>
      end
    end
  end

  defp binary_form(%Type.Struct{packed: size}, length) when is_integer(size) do
    if length do
      quote context: Elixir do
        <<_::unquote(size * 8 * length)>>
      end
    else
      quote context: Elixir do
        <<_::_*unquote(size * 8)>>
      end
    end
  end

  defp binary_form(_, _), do: nil

  def of(type, len, opts \\ []) do
    struct(__MODULE__, opts ++ [child: type, len: len])
  end
end
