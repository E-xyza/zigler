defmodule Zig.Attributes do
  @moduledoc false

  @reserved_attributes ~w[
  after_compile after_verify before_compile behaviour impl compile
  deprecated doc typedoc dialyzer external_resource moduledoc on_definition
  on_load spec type behaviour callback macrocallback
  ]a

  def from_module(module) do
    module
    |> Module.attributes_in()
    |> Enum.reject(&(&1 in @reserved_attributes))
    |> Enum.map(&{&1, Module.get_attribute(module, &1)})
    |> Enum.filter(&usable?(elem(&1, 1)))
  end

  def code_path(module) do
    module.module_code_path
    |> Path.dirname()
    |> Path.join("attributes.zig")
  end

  defguardp is_primitive(value)
            when is_integer(value) or is_float(value) or is_binary(value) or is_atom(value)

  defp usable?(value) when is_primitive(value), do: true

  defp usable?(value) when is_tuple(value) do
    value
    |> Tuple.to_list()
    |> Enum.all?(&usable?/1)
  end

  defp usable?(_), do: false

  def render_zig({name, value}) do
    "pub const #{name} = #{render_zig_from_term(value)};\n"
  end

  defp render_zig_from_term(value)
       when is_integer(value) or is_float(value) or is_boolean(value) do
    "#{value}"
  end

  defp render_zig_from_term(value) when is_nil(value), do: "null"

  defp render_zig_from_term(value) when is_binary(value) do
    ~s("#{value}")
  end

  defp render_zig_from_term(value) when is_atom(value) do
    ".@\"#{value}\""
  end

  defp render_zig_from_term(value) when is_tuple(value) do
    tuple_items =
      value
      |> Tuple.to_list()
      |> Enum.map_join(", ", &render_zig_from_term/1)

    ".{#{tuple_items}}"
  end
end
