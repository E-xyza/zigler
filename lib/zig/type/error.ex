defmodule Zig.Type.Error do
  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module),
    do: %__MODULE__{child: Type.from_json(child, module)}

  def get_allowed?(error), do: Type.get_allowed?(error.child)
  def make_allowed?(error), do: Type.make_allowed?(error.child)
  def can_cleanup?(_), do: false

  def marshal_param(_, _, _, _), do: raise("unreachable")

  def marshal_return(error, variable, lang) do
    Type.marshal_return(error.child, variable, lang)
  end

  def binary_size(type), do: Type.binary_size(type.child)

  def render_payload_options(_, _, _), do: raise("unreachable")

  def render_zig(error), do: "!#{error.child}"

  def render_elixir_spec(error, context) do
    Type.render_elixir_spec(error.child, context)
  end

  def of(child), do: %__MODULE__{child: child}
end
