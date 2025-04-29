defmodule Zig.Parameter do
  @moduledoc false

  @enforce_keys ~w[cleanup in_out]a
  defstruct @enforce_keys ++ [:type]

  alias Zig.Options
  alias Zig.Type

  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          in_out: boolean
        }

  @type opts :: :noclean | [:noclean | {:cleanup, boolean}]

  def new(options, module) do
    struct!(__MODULE__, normalize_options(options, module))
  end

  @options ~w[cleanup in_out]a

  def normalize_options(options, module) do
    options
    |> List.wrap()
    |> Enum.flat_map(fn
      :noclean ->
        [cleanup: false]

      :in_out ->
        [in_out: true, cleanup: false]

      {k, v} when k in @options and is_boolean(v) ->
        [{k, v}]

      {k, v} when k in @options ->
        Options.raise_with(
          "nif parameter option `#{k}` must be boolean",
          v,
          module
        )

      {k, _} ->
        Options.raise_with(
          "nif parameter option key `#{k}` is invalid",
          module
        )

      other ->
        Options.raise_with(
          "nif parameter option `#{inspect(other)}` is invalid",
          module
        )
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
      parameter.cleanup ->
        Type.render_cleanup(parameter.type, index)

      :else ->
        ".{.cleanup = false},"
    end
  end

  def merge(sema, spec), do: %{sema | cleanup: spec.cleanup, in_out: spec.in_out}
end
