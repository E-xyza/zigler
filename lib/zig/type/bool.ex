defmodule Zig.Type.Bool do
  @moduledoc false

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
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(_), do: 1

  @impl true
  def render_zig(_), do: "bool"

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()
  @impl true
  def payload_options(_, _), do: Type._default_payload_options()
  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()
  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)

  @impl true
  def render_elixir_spec(_, _) do
    quote do
      boolean()
    end
  end
end
