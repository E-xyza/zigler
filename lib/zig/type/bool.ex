defmodule Zig.Type.Bool do
  alias Zig.Type
  use Type

  defstruct []

  @type t :: %__MODULE__{}

  def from_json(_), do: %__MODULE__{}

  def return_allowed?(_), do: true

  def spec(_, _, _), do: Type.spec(:boolean)
end
