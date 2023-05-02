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

  def spec(%{child: ~t(u8)}, :return, opts) do
    Type.spec(if :charlist in opts, do: :charlist, else: :binary)
  end

  def spec(%{child: child = %Type.Integer{bits: size}}, :return, opts) do
    if :binary in opts do
      quote context: Elixir do
        <<_::_*unquote(Type.Integer._next_power_of_two_ceil(size))>>
      end
    else
      list(child, :return, opts)
    end
  end

  def spec(%{child: child = %Type.Float{bits: size}}, :return, opts) do
    if :binary in opts do
      quote context: Elixir do
        <<_::_*unquote(size)>>
      end
    else
      list(child, :return, opts)
    end
  end

  def spec(%{child: child}, :return, opts), do: list(child, :return, opts)

  defp list(child, context, opts) do
    quote context: Elixir do
      [unquote(Type.spec(child, context, opts))]
    end
  end

  def return_allowed?(slice), do: Type.return_allowed?(slice.child)

  def of(child), do: %__MODULE__{child: child}
end
