defmodule Zig.Type.Bool do
  alias Zig.Type
  use Type

  defstruct []

  @type t :: %__MODULE__{}

  def from_json(_), do: %__MODULE__{}

  def to_string(_), do: "bool"
  def to_call(_), do: "bool"

  def return_allowed?(_), do: true

  def spec(_, _, _), do: Type.spec(:boolean)
end
