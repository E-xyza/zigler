defmodule Zig.Type.Bool do
  alias Zig.Type
  use Type

  defstruct []

  @type t :: %__MODULE__{}

  def from_json(_), do: %__MODULE__{}

  @impl true
  def get_allowed?(_), do: true
  @impl true
  def make_allowed?(_), do: true
  @impl true
  def binary_size(_), do: 1

  @impl true
  def render_zig(_), do: "bool"

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()
  @impl true
  def render_payload_options(_, _, _), do: Type._default_payload_options()
  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()
  @impl true
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  @impl true
  def marshal_return(_, _, _), do: Type._default_marshal()

  @impl true
  def render_elixir_spec(_, _) do
    quote do
      boolean()
    end
  end
end
