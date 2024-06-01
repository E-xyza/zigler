defmodule Zig.Type.Error do
  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module),
    do: %__MODULE__{child: Type.from_json(child, module)}

  def make_allowed?(optional), do: Type.make_allowed?(optional.child)
  def can_cleanup?(_), do: false

  def render_zig(%{child: child}), do: "!#{child}"

  def render_elixir_spec(%{child: child}, context, opts) do
    Type.render_elixir_spec(child, context, opts)
  end

  def render_return(_, _), do: Type._default_return()

  def of(child), do: %__MODULE__{child: child}
end
