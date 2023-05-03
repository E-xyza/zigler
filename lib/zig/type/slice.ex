defmodule Zig.Type.Slice do
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

  def to_string(%{has_sentinel?: true, repr: repr}), do: repr
  def to_string(slice), do: "[]#{Kernel.to_string(slice.child)}"

  def to_call(%{has_sentinel?: true, repr: repr}), do: repr
  def to_call(slice), do: "[]#{Type.to_call(slice.child)}"

  # TYPE SPEC STUFF

  def spec(%{child: ~t(u8)}, :return, opts) do
    if Keyword.fetch!(opts, :type) == :charlist,
      do: [Type.spec(~t(u8), :return, opts)],
      else: binary_form(~t(u8))
  end

  def spec(%{child: ~t(u8)}, :params, opts) do
    quote context: Elixir do
      [unquote(Type.spec(~t(u8), :params, opts))] | unquote(binary_form(~t(u8)))
    end
  end

  def spec(%{child: child}, context, opts) do
    case {context, Keyword.fetch!(opts, :type)} do
      {:return, :binary} ->
        binary_form(child) || raise "unreachable"

      {:return, type} when type in ~w(default charlist)a ->
        [Type.spec(child, :return, [])]

      {:params, _} ->
        if binary_form(child) do
          quote context: Elixir do
            unquote([Type.spec(child, :params, [])]) | unquote(binary_form(child))
          end
        else
          [Type.spec(child, :params, [])]
        end
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

  # ETC

  def return_allowed?(slice), do: Type.return_allowed?(slice.child)

  def of(child, opts \\ []), do: struct(__MODULE__, [child: child] ++ opts)
end
