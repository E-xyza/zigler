defmodule Zig.Type.Bool do
  alias Zig.Type
  use Type

  defstruct []

  @type t :: %__MODULE__{}

  def from_json(_), do: %__MODULE__{}

  def get_allowed?(_), do: true
  def make_allowed?(_), do: true
  def can_cleanup?(_), do: false
  def binary_size(_), do: 1

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()

  def render_elixir_spec(_, _) do
    quote do
      boolean()
    end
  end
end
