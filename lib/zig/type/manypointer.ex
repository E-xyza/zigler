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

  def to_string(%{has_sentinel?: true, repr: repr}), do: repr
  def to_string(slice), do: "[*]#{Kernel.to_string(slice.child)}"

  def to_call(%{has_sentinel?: true, repr: repr}), do: repr
  def to_call(slice), do: "[*]#{Type.to_call(slice.child)}"

  def return_allowed?(pointer), do: pointer.has_sentinel? and Type.return_allowed?(pointer.child)
  def missing_size?(_), do: true

  def spec(%{child: ~t(u8), has_sentinel?: true}, :return, opts) do
    if :charlist in opts do
      Type.spec(:charlist)
    else
      Type.spec(:binary)
    end
  end

  def of(type, opts) do
    struct(__MODULE__, opts ++ [child: type])
  end
end
