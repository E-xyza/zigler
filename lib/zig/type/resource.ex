defmodule Zig.Type.Resource do
  alias Zig.Type
  use Type

  defstruct [:payload, :name]

  @type t :: %__MODULE__{payload: Type.t(), name: String.t()}

  def from_json(%{"payload" => payload, "name" => name}, module) do
    name =
      name
      |> String.replace("resource.Resource", "Resource")
      |> String.replace(",sema,", ",root,")
      |> String.replace(".#{module}", "nif")

    %__MODULE__{
      payload: Type.from_json(payload, module),
      name: name
    }
  end

  def to_string(resource), do: resource.name

  def to_call(resource), do: resource.name

  def return_allowed?(_resource), do: true
end
