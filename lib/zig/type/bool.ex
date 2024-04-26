defmodule Zig.Type.Bool do
  alias Zig.Type
  use Type

  defstruct []

  @type t :: %__MODULE__{}

  def from_json(_), do: %__MODULE__{}

  def return_allowed?(_), do: true

  def render_payload_options(type, index, _), do: Type._default_payload_options()
  def render_return(type), do: Type._default_return()

  def marshals_param?(_), do: false
  def marshals_return?(_), do: false

  def spec(_, _, _), do: Type.spec(:boolean)
end
