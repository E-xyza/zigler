defmodule Zig.Type.Pointer do
  @moduledoc false

  alias Zig.Type
  alias Zig.Type.Array
  alias Zig.Type.Optional
  alias Zig.Type.Struct

  use Type

  @enforce_keys ~w[child const]a
  defstruct @enforce_keys ++ [optional: false]

  @type t :: %__MODULE__{
          optional: boolean(),
          const: boolean(),
          child: Type.t()
        }

  @mutable_types ~w[array struct]

  # special case: pointer represents that the data are mutable.
  def from_json(
        %{"type" => "pointer", "child" => %{"type" => mutable} = child, "is_const" => false},
        module
      )
      when mutable in @mutable_types do
    child
    |> Type.from_json(module)
    |> struct!(mutable: true)
  end

  # special case: pointer represents that the data are mutable AND optional.
  def from_json(
        %{
          "type" => "optional",
          "child" => %{
            "type" => "pointer",
            "child" => %{"type" => mutable} = child,
            "is_const" => false
          }
        },
        module
      )
      when mutable in @mutable_types do
    child
    |> Type.from_json(module)
    |> struct!(mutable: true)
    |> then(&%Optional{child: &1})
  end

  def from_json(%{"type" => "optional", "child" => %{"type" => "pointer"} = pointer}, module) do
    pointer
    |> from_json(module)
    |> Map.replace!(:optional, true)
  end

  def from_json(
        %{"type" => "pointer", "child" => %{"type" => "unusable:anyopaque"}, "is_const" => const},
        _module
      ) do
    %__MODULE__{child: :anyopaque, const: const}
  end

  def from_json(%{"child" => child, "is_const" => const}, module) do
    %__MODULE__{child: Type.from_json(child, module), const: const}
  end

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)

  @impl true
  def marshal_return(_, _, _), do: raise("unreachable")

  # validations:

  @impl true
  def get_allowed?(_), do: false

  @impl true
  def make_allowed?(%{child: %Array{} = array}), do: Type.make_allowed?(array)
  def make_allowed?(%{child: %Struct{} = struct}), do: Type.make_allowed?(struct)
  def make_allowed?(_), do: false

  @impl true
  def in_out_allowed?(%{child: child}), do: Type.get_allowed?(child) and Type.make_allowed?(child)

  @impl true
  def binary_size(_), do: nil

  @impl true
  def render_accessory_variables(type, param, prefix) do
    if param.in_out do
      ~s(var #{prefix}: #{Type.render_zig(type.child)} = undefined;)
    else
      raise "unreachable"
    end
  end

  @impl true
  # pointer might be used as a in-out return value.
  def payload_options(_, _), do: [error_info: "&error_info"]

  @impl true
  def render_zig(%{optional: true} = type), do: "?*#{Type.render_zig(type.child)}"

  def render_zig(type), do: "*#{Type.render_zig(type.child)}"

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def render_elixir_spec(type, context), do: Type.render_elixir_spec(type.child, context)
end
