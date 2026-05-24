defmodule Zig.Type.Tuple do
  @moduledoc false

  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type

  use Type

  defstruct [:name, :elements]

  @type t :: %__MODULE__{
          name: String.t(),
          elements: [Type.t()]
        }

  def from_json(%{"name" => name, "fields" => fields}, module) do
    # Fields are in order already for tuples (named "0", "1", "2", etc.)
    elements =
      fields
      |> Enum.sort_by(& &1["name"])
      |> Enum.map(fn desc -> Type.from_json(desc["type"], module) end)

    %__MODULE__{
      name: String.trim_leading(name, ".#{module}."),
      elements: elements
    }
  end

  @impl true
  def render_elixir_spec(tuple, %Return{as: as}) do
    render_elixir_spec(tuple, as)
  end

  def render_elixir_spec(tuple, %Parameter{}) do
    render_tuple_spec(tuple)
  end

  def render_elixir_spec(tuple, _context) do
    render_tuple_spec(tuple)
  end

  defp render_tuple_spec(%{elements: elements}) do
    element_specs = Enum.map(elements, &Type.render_elixir_spec(&1, :default))

    quote do
      {unquote_splicing(element_specs)}
    end
  end

  @impl true
  def render_zig(%{name: name}), do: "nif.#{name}"

  @impl true
  def get_allowed?(tuple) do
    Enum.all?(tuple.elements, &Type.get_allowed?/1)
  end

  @impl true
  def make_allowed?(tuple) do
    Enum.all?(tuple.elements, &Type.make_allowed?/1)
  end

  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(_), do: nil

  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()

  @impl true
  def needs_size?(_), do: false

  @impl true
  def payload_options(_, _), do: Type._default_payload_options()

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)

  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)
end
