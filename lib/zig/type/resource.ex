defmodule Zig.Type.Resource do
  alias Zig.Type
  use Type

  defstruct [:payload]

  @type t :: %__MODULE__{payload: Type.t()}

  def from_json(%{"payload" => payload}, module) do
    %__MODULE__{
      payload: Type.from_json(payload, module)
    }
  end

  def to_string(resource), do: "Resource(#{resource.payload})"

  def to_call(resource), do: "Resource(#{Type.to_call(resource.payload)})"

  def return_allowed?(_resource), do: true
end
