defmodule Zig.Type.Optional do
  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module),
    do: %__MODULE__{child: Type.from_json(child, module)}

  def return_allowed?(optional), do: Type.return_allowed?(optional.child)

  def spec(%{child: child}, context, opts) do
    quote do
      unquote(Type.spec(child, context, opts)) | nil
    end
  end

  def of(child), do: %__MODULE__{child: child}
end
