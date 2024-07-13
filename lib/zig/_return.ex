defmodule Zig.Return do
  @moduledoc false

  @enforce_keys ~w[type cleanup]a
  defstruct @enforce_keys ++ [:in_out, :error, spec: nil, as: :default]

  alias Zig.Type

  @type type :: :binary | :integer | :default | :list | {:list, type}

  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          as: type,
          spec: Macro.t(),
          in_out: nil | String.t(),
          error: atom()
        }

  @type opts :: [
          :noclean
          | :binary
          | :list
          | {:cleanup, boolean}
          | {:as, type}
          | {:in_out, String.t()}
          | {:error, atom()}
        ]

  def new(raw) when raw in ~w[term erl_nif_term]a, do: %__MODULE__{type: raw, cleanup: false}

  def new(type, options) do
    struct!(__MODULE__, [type: type] ++ normalize_options(options))
  end

  @as ~w[binary list integer map]a
  @options ~w[as cleanup spec in_out error]a

  defp normalize_options(options) do
    options
    |> List.wrap()
    |> Enum.map(fn
      option when option in @as ->
        {:as, option}

      :noclean ->
        {:cleanup, false}

      {:list, _} = v ->
        {:as, v}

      {:map, _} = v ->
        {:as, v}

      {k, _} = kv when k in @options ->
        kv
    end)
    |> Keyword.put_new(:cleanup, true)
  end

  def render_return(%{in_out: in_out_var, error: nil}) when is_binary(in_out_var) do
    "_ = result; break :execution_block beam.make(#{in_out_var}, .{}).v;"
  end

  def render_return(%{in_out: in_out_var, error: error_fn}) when is_binary(in_out_var) do
    """
    nif.#{error_fn}(result) catch |err| {
        break :execution_block beam.raise_exception(err, .{}).v;
    };

    break :execution_block beam.make(#{in_out_var}, .{}).v;
    """
  end

  def render_return(%{type: :void}),
    do: "_ = result; break :execution_block beam.make(.ok, .{}).v;"

  def render_return(%{as: type}),
    do: "break :execution_block beam.make(result, .{.as = #{render_return_as(type)}}).v;"

  defp render_return_as(atom) when is_atom(atom), do: ".#{atom}"
  defp render_return_as({:list, return}), do: ".{.list = #{render_return_as(return)}}"

  defp render_return_as({:map, map_kv_list}),
    do: ".{.map = .{#{render_map_kv_list(map_kv_list)}}}"

  defp render_map_kv_list(map_kv_list) do
    Enum.map_join(map_kv_list, ", ", fn {key, value} ->
      ".#{key} = #{render_return_as(value)}"
    end)
  end
end
