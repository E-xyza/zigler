defmodule Zig.Parameter do
  @moduledoc false

  @enforce_keys ~w[type cleanup in_out]a
  defstruct @enforce_keys

  alias Zig.Type

  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          in_out: boolean
        }

  @type opts :: :noclean | [:noclean | {:cleanup, boolean}]

  def new(type, options) do
    struct!(__MODULE__, [type: type] ++ normalize_options(options))
  end

  @options ~w[cleanup]a

  def normalize_options(options) do
    options
    |> List.wrap()
    |> Enum.map(fn
      :noclean -> {:cleanup, false}
      :in_out -> {:in_out, true}
      {k, _} = kv when k in @options -> kv
    end)
    |> then(&Keyword.merge([cleanup: true, in_out: false], &1))
  end

  def render_accessory_variables(parameter, index) do
    Type.render_accessory_variables(parameter.type, parameter, "arg#{index}")
  end

  def render_payload_options(parameter, index) do
    parameter.type
    |> Type.payload_options("arg#{index}")
    |> then(fn
      options when parameter.in_out -> Keyword.put(options, :in_out, "&arg#{index}")
      options -> options
    end)
    |> assemble_payload_options
  end

  defp assemble_payload_options(options) do
    [".{", Enum.map(options, fn {k, v} -> ~s(.#{k} = #{v},) end), "},"]
  end

  def render_cleanup(parameter, index) do
    cond do
      parameter.in_out ->
        ".{.cleanup = false},"

      parameter.cleanup ->
        Type.render_cleanup(parameter.type, index)

      :else ->
        ".{.cleanup = false},"
    end
  end
end
