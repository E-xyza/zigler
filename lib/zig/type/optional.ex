defmodule Zig.Type.Optional do
  @moduledoc false

  alias Zig.Type
  use Type

  defstruct [:child]

  @type t :: %__MODULE__{child: Type.t()}

  def from_json(%{"child" => child}, module) do
    %__MODULE__{child: Type.from_json(child, module)}
  end

  @impl true
  def get_allowed?(optional), do: Type.get_allowed?(optional.child)
  @impl true
  def make_allowed?(optional), do: Type.make_allowed?(optional.child)
  @impl true
  def in_out_allowed?(optional), do: Type.in_out_allowed?(optional.child)

  @impl true
  def binary_size(optional), do: Type.binary_size(optional.child)

  @impl true
  def render_zig(optional), do: "?#{Type.render_zig(optional.child)}"

  @impl true
  def payload_options(optional, prefix), do: Type.payload_options(optional.child, prefix)
  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)

  @impl true
  def render_cleanup(optional, index), do: Type.render_cleanup(optional.child, index)

  @impl true
  def needs_size?(optional), do: Type.needs_size?(optional.child)

  @impl true
  def render_elixir_spec(optional, context) do
    quote do
      unquote(Type.render_elixir_spec(optional.child, context)) | nil
    end
  end

  @impl true
  def render_erlang_spec(optional, context) do
    "#{Type.render_erlang_spec(optional.child, context)} | nil"
  end

  def of(child), do: %__MODULE__{child: child}
end
