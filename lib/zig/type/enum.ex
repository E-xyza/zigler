defmodule Zig.Type.Enum do
  @moduledoc false

  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  use Type

  defstruct [:tags, :name]
  @type t :: %__MODULE__{tags: %{optional(atom) => String.t()}, name: String.t()}

  def from_json(%{"tags" => tags, "name" => name}, module) do
    %__MODULE__{
      tags: Map.new(tags, fn {key, val} -> {String.to_atom(key), val} end),
      name: String.trim_leading(name, ".#{module}.")
    }
  end

  def inspect(enum, opts) do
    ~s(%Zig.Type.Enum{name: "#{enum.name}", tags: #{Kernel.inspect(enum.tags, opts)}})
  end

  @impl true
  def get_allowed?(_), do: true
  @impl true
  def make_allowed?(_), do: true
  @impl true
  def in_out_allowed?(_), do: false

  @impl true
  def binary_size(_), do: nil

  @impl true
  def marshal_param(_, variable, _, platform), do: Type._default_marshal_param(platform, variable)
  @impl true
  def marshal_return(_, variable, platform), do: Type._default_marshal_return(platform, variable)

  @impl true
  def render_accessory_variables(_, _, _), do: Type._default_accessory_variables()

  @impl true
  def payload_options(_, _), do: Type._default_payload_options()
  @impl true
  def render_cleanup(_, _), do: Type._default_cleanup()
  @impl true
  def render_zig(%{name: name}), do: name

  @impl true
  def render_elixir_spec(%{tags: tags}, %Parameter{}) do
    tags
    |> integers()
    |> Kernel.++(atoms(tags))
    |> unionize
  end

  def render_elixir_spec(type, %Return{as: as}), do: render_elixir_spec(type, as)

  def render_elixir_spec(type, :integer) do
    type.tags
    |> integers()
    |> unionize
  end

  def render_elixir_spec(type, :default) do
    type.tags
    |> atoms()
    |> unionize
  end

  defp integers(tags) do
    tags
    |> Map.values()
    |> Enum.sort(:desc)
    |> Enum.reduce([], &accumulate/2)
  end

  defp atoms(tags) do
    tags
    |> Map.keys()
    |> Enum.sort(:asc)
  end

  defp accumulate(number, []), do: [number]
  defp accumulate(number, [succ | rest]) when succ == number + 1, do: [number..succ | rest]

  defp accumulate(number, [succ..last//1 | rest]) when succ == number + 1,
    do: [number..last | rest]

  defp accumulate(number, noncontiguous), do: [number | noncontiguous]

  defp unionize(content) do
    content
    |> Enum.map(&rerender/1)
    |> Enum.reduce(fn a, b ->
      quote do
        unquote(a) | unquote(b)
      end
    end)
  end

  defp rerender(a..b//1) do
    quote do
      unquote(a)..unquote(b)
    end
  end

  defp rerender(number), do: number
end
