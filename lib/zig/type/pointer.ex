defmodule Zig.Type.Pointer do
  alias Zig.Type
  use Type
  @behaviour Type

  defstruct [:child, optional: false]

  @type t :: %__MODULE__{
          optional: boolean(),
          child: Type.t()
        }

  def from_json(%{"type" => "optional", "child" => %{"child" => child}}, module) do
    %__MODULE__{optional: true, child: Type.from_json(child, module)}
  end

  def from_json(%{"child" => child}, module) do
    %__MODULE__{child: Type.from_json(child, module)}
  end

  @impl true
  def marshal_param(_, _, _, _), do: raise("unreachable")

  @impl true
  def marshal_return(_, _, _), do: raise("unreachable")

  # validations:

  @impl true
  def get_allowed?(_), do: false

  @impl true
  def make_allowed?(_), do: false

  @impl true
  def can_cleanup?(_), do: raise("unreachable")

  @impl true
  def render_payload_options(_, _, _), do: raise("unreachable")

  @impl true
  def render_return(_, _), do: raise("unreachable")

  @impl true
  def render_zig(%{optional: true, child: child}), do: "?*#{Type.render_zig(child)}"

  def render_zig(%{child: child}), do: "?#{Type.render_zig(child)}"

  @impl true
  def render_elixir_spec(_, _, _), do: raise("unreachable")
end
