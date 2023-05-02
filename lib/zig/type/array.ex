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

  def spec(%{child: ~t(u8), len: length, has_sentinel?: false}, _) do
    quote context: Elixir do
      <<_::unquote(length * 8)>>
    end
  end

  def spec(%{child: child}, opts) do
    quote context: Elixir do
      [unquote(Type.spec(child, opts))]
    end
  end

  def of(type, len, opts \\ []) do
    struct(__MODULE__, opts ++ [child: type, len: len])
  end
end
