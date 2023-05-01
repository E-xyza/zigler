defmodule Zig.Type.Slice do
  alias Zig.Type
  use Type

  defstruct [:child, :has_sentinel?, :repr]

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

  def return_allowed?(slice), do: Type.return_allowed?(slice.child)
end
