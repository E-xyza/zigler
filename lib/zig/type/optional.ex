defmodule Zig.Type.Optional do
  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module) do
    %__MODULE__{child: Type.from_json(child, module)}
  end

  def get_allowed?(optional), do: Type.get_allowed?(optional.child)
  def make_allowed?(optional), do: Type.make_allowed?(optional.child)
  def can_cleanup?(optional), do: Type.can_cleanup?(optional.child)
  def binary_size(optional), do: Type.binary_size(optional.child)

  def render_zig(optional), do: "?#{Type.render_zig(optional.child)}"

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()

  def render_elixir_spec(optional, context) do
    quote do
      unquote(Type.render_elixir_spec(optional.child, context)) | nil
    end
  end

  def of(child), do: %__MODULE__{child: child}
end
