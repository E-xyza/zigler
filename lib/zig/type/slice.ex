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

  def spec(%{child: ~t(u8)}, _), do: binary()
  def spec(%{child: child}, opts), do: list(child, opts)

  defp binary do
    quote context: Elixir do
      binary()
    end
  end

  defp list(child, opts) do
    quote context: Elixir do
      [unquote(Type.spec(child, opts))]
    end
  end

  def return_allowed?(slice), do: Type.return_allowed?(slice.child)

  def of(child), do: %__MODULE__{child: child}
end
