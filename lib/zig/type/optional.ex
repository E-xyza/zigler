defmodule Zig.Type.Optional do
  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module),
    do: %__MODULE__{child: Type.from_json(child, module)}

  # TODO: punt these to child's functions
  def marshal_param(_, _), do: nil

  def marshal_return(_, _), do: nil

  def param_errors(_type, _opts) do
  end

  def to_string(optional), do: "?#{Kernel.to_string(optional.child)}"

  def to_call(optional), do: "?#{Type.to_call(optional.child)}"
end
