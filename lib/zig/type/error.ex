defmodule Zig.Type.Error do
  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module),
    do: %__MODULE__{child: Type.from_json(child, module)}

  def to_string(optional), do: "!#{Kernel.to_string(optional.child)}"

  def to_call(optional), do: "!#{Type.to_call(optional.child)}"

  def return_allowed?(optional), do: Type.return_allowed?(optional.child)

  def spec(%{child: child}, context, opts) do
    Type.spec(child, context, opts)
  end

  def of(child), do: %__MODULE__{child: child}
end
