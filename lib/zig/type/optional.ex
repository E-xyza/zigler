defmodule Zig.Type.Optional do
  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module),
    do: %__MODULE__{child: Type.from_json(child, module)}

  def param_allowed?(optional), do: Type.param_allowed?(optional.child)
  def return_allowed?(optional), do: Type.return_allowed?(optional.child)
  def can_cleanup?(optional), do: Type.can_cleanup?(optional.child)

  def render_zig(%{child: child}), do: "?#{Type.render_zig(child)}"

  def render_payload_options(_, _, _), do: Type._default_payload_options()
  def render_return(_, _), do: Type._default_return()
  def marshal_param(_, _, _, _), do: Type._default_marshal()
  def marshal_return(_, _, _), do: Type._default_marshal()

  def render_elixir_spec(%{child: child}, context, opts) do
    quote do
      unquote(Type.render_elixir_spec(child, context, opts)) | nil
    end
  end

  def of(child), do: %__MODULE__{child: child}
end
