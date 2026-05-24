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

  def render_elixir_spec(tuple, %Parameter{} = params) do
    render_tuple_spec(tuple, params)
  end

  def render_elixir_spec(tuple, context) do
    render_tuple_spec(tuple, context)
  end

  @impl true
  def render_erlang_spec(tuple, %Return{as: as}), do: render_erlang_spec(tuple, as)

  def render_erlang_spec(tuple, %Parameter{} = params), do: render_erlang_tuple_spec(tuple, params)
  def render_erlang_spec(tuple, context), do: render_erlang_tuple_spec(tuple, context)

  defp render_erlang_tuple_spec(%{elements: elements}, context) do
    element_specs =
      elements
      |> Enum.with_index()
      |> Enum.map_join(", ", fn {element, index} ->
        element_opts = element_opts(context, index)
        Type.render_erlang_spec(element, element_opts)
      end)

    "{#{element_specs}}"
  end

  defp render_tuple_spec(%{elements: elements}, context) do
    element_specs =
      elements
      |> Enum.with_index()
      |> Enum.map(fn {element, index} ->
        element_opts = element_opts(context, index)
        Type.render_elixir_spec(element, element_opts)
      end)

    quote do
      {unquote_splicing(element_specs)}
    end
  end

  defp element_opts({:tuple, list}, index) when is_list(list) do
    Enum.find_value(list, :default, fn
      {^index, type} -> type
      _ -> nil
    end)
  end

  defp element_opts(%Parameter{} = params, _index), do: params

  defp element_opts(_, _), do: :default

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
