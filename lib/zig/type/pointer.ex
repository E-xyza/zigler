defmodule Zig.Type.Pointer do
  alias Zig.Type
  alias Zig.Type.Optional
  
  use Type

  @behaviour Type

  defstruct [:child, optional: false]

  @type t :: %__MODULE__{
          optional: boolean(),
          child: Type.t()
        }

  
  @mutable_types ~w(array struct)

  # special case: pointer represents that the data are mutable.
  def from_json(%{"type" => "pointer", "child" => %{"type" => mutable} = child}, module) when mutable in @mutable_types do
    child
    |> Type.from_json(module)
    |> Map.replace!(:mutable, true)
  end

  # special case: pointer represents that the data are mutable AND optional.
  def from_json(%{"type" => "optional", "child" => %{"type" => "pointer", "child" => %{"type" => mutable} = child}}, module) when mutable in @mutable_types do
    child
    |> Type.from_json(module)
    |> Map.replace!(:mutable, true)
    |> then(&%Optional{child: &1})
  end

  def from_json(%{"type" => "optional", "child" => %{"child" => %{"type" => "unusable:anyopaque"}}}, _module) do
    %Optional{child: :anyopaque_pointer}
  end

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
  def can_cleanup?(_), do: false
  
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
