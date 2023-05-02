defmodule Zig.Type.Cpointer do
  alias Zig.Type
  alias Zig.Type.Struct
  use Type

  import Type, only: :macros

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module) do
    %__MODULE__{child: Type.from_json(child, module)}
  end

  def to_string(slice), do: "[*c]#{Kernel.to_string(slice.child)}"

  def to_call(slice), do: "[*c]#{Type.to_call(slice.child)}"

  def return_allowed?(pointer) do
    case pointer.child do
      ~t(u8) -> true
      # null-terminated list of pointers.
      %__MODULE__{child: child} -> Type.return_allowed?(child)
      # NB: we assume these are single pointer returns.
      struct = %Struct{} -> Type.return_allowed?(struct)
      _ -> false
    end
  end

  def missing_size?(_), do: true

  def of(child), do: %__MODULE__{child: child}
end
