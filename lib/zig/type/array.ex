defmodule Zig.Type.Array do
  alias Zig.Type
  use Type

  @derive Inspect
  defstruct [:child, :len]
  @type t :: %__MODULE__{
    child: Type.t,
    len: non_neg_integer
  }

  def from_json(%{"child" => child, "len" => len}, module) do
    %__MODULE__{child: Type.from_json(child, module), len: len}
  end

  def marshal_param(_), do: nil

  def marshal_return(_), do: nil

  def param_errors(_), do: nil

  def to_string(array), do: "[#{array.len}]#{Kernel.to_string(array.child)}"

  def to_call(array), do: "[#{array.len}]#{Type.to_call(array.child)}"
end
