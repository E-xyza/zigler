defmodule Zig.Type.Bool do
  alias Zig.Type
  use Type

  defstruct []

  @type t :: %__MODULE__{}

  def from_json(_), do: %__MODULE__{}

  def param_allowed?(_), do: true
  def return_allowed?(_), do: true
  def can_cleanup?(_), do: false

  def render_payload_options(type, index, _), do: Type._default_payload_options()
  def render_return(type, _), do: Type._default_return()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()

  def spec(_, _, _), do: Type.spec(:boolean)
end
