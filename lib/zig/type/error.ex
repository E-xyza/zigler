defmodule Zig.Type.Error do
  @moduledoc false

  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module),
    do: %__MODULE__{child: Type.from_json(child, module)}

  @impl true
  def get_allowed?(_), do: false
  @impl true
  def make_allowed?(error), do: Type.make_allowed?(error.child)
  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def marshal_param(_, _, _, _), do: raise("unreachable")

  @impl true
  def marshal_return(error, variable, lang) do
    Type.marshal_return(error.child, variable, lang)
  end

  @impl true
  def binary_size(type), do: Type.binary_size(type.child)

  @impl true
  def render_accessory_variables(_, _, _), do: raise("unreachable")

  @impl true
  def payload_options(_, _), do: raise("unreachable")

  @impl true
  def render_zig(error), do: "!#{error.child}"

  @impl true
  def render_cleanup(_, _), do: raise("unreachable")

  @impl true
  def render_elixir_spec(error, context) do
    Type.render_elixir_spec(error.child, context)
  end

  def of(child), do: %__MODULE__{child: child}
end
