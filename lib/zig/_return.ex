defmodule Zig.Return do
  @moduledoc false

  @enforce_keys ~w[type cleanup]a
  defstruct @enforce_keys ++ [:in_out, :error, :length, spec: nil, as: :default]

  alias Zig.Type

  @type type :: :binary | :integer | :default | :list | {:list, type}

  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          as: type,
          spec: Macro.t(),
          in_out: nil | String.t(),
          error: atom(),
          length: non_neg_integer | {:arg, non_neg_integer()}
        }

  @type opts :: [
          :noclean
          | :binary
          | :list
          | {:cleanup, boolean}
          | {:as, type}
          | {:in_out, atom()}
          | {:error, atom()}
          | {:length, non_neg_integer | {:arg, non_neg_integer()}}
        ]

  def new(:raw, raw) when raw in ~w[term erl_nif_term]a,
    do: %__MODULE__{type: raw, cleanup: false}

  def new(type, options) do
    struct!(__MODULE__, [type: type] ++ options)
  end

  @as ~w[binary list integer map]a
  @options ~w[as cleanup spec in_out error length]a

  def normalize_options(options, cleanup, module) do
    options
    |> List.wrap()
    |> Enum.map(fn
      option when option in @as ->
        {:as, option}

      {:as, type} ->
        {:as, validate_type(type, [])}

      :noclean ->
        {:cleanup, false}

      {:list, _} = v ->
        {:as, validate_type(v, [])}

      {:map, _} = v ->
        {:as, validate_type(v, [])}

      {:length, int} when is_integer(int) and int >= 0 ->
        {:length, int}

      {:length, {:arg, arg}} when is_integer(arg) and arg >= 0 ->
        {:length, {:arg, arg}}

      {:length, v} ->
        raise CompileError,
          description:
            "nif option `length` must be a non-negative integer or an argument spec, got: `#{inspect(v)}`",
          file: module.file,
          line: module.line

      {:cleanup, cleanup} when is_boolean(cleanup) ->
        {:cleanup, cleanup}

      {:spec, spec} when is_atom(spec) or is_tuple(spec) or is_list(spec) ->
        {:spec, spec}

      {:in_out, in_out} when is_atom(in_out) ->
        {:in_out, in_out}

      {:in_out, in_out} ->
        raise CompileError,
          description: "nif option `in_out` must be an atom, got: `#{inspect(in_out)}`",
          file: module.file,
          line: module.line

      {:error, error} when is_atom(error) ->
        {:error, error}

      {:error, error} ->
        raise CompileError,
          description: "nif option `error` must be a module, got: `#{inspect(error)}`",
          file: module.file,
          line: module.line
    end)
    |> Keyword.put_new(:cleanup, cleanup)
  catch
    {:deep_typeerror, wrong, stack} ->
      raise CompileError,
        description: "nif option `as` is invalid, got: `#{inspect(wrong)}` @ [#{unwind(stack)}]",
        file: module.file,
        line: module.line
  end

  defp validate_type(type, _) when type in @as, do: type

  defp validate_type({:list, type} = list, stack) do
    validate_type(type, [:list | stack])
    list
  end

  defp validate_type({:map, typemap} = map, stack) do
    Enum.each(typemap, fn {key, value} ->
      validate_type(value, [{:map, key} | stack])
    end)

    map
  end

  defp validate_type(wrong, stack) do
    throw {:deep_typeerror, wrong, stack}
  end

  defp unwind(list) do
    list
    |> Enum.reverse()
    |> Enum.map_join(" > ", fn
      :list -> "list"
      {:map, key} -> "map(#{key})"
    end)
  end

  def render_return(%{in_out: in_out_var, error: nil} = return) when is_binary(in_out_var) do
    "_ = result; break :execution_block beam.make(#{in_out_var}, .{#{return_opts(return)}}).v;"
  end

  def render_return(%{in_out: in_out_var, error: error_fn} = return) when is_binary(in_out_var) do
    """
    nif.#{error_fn}(result) catch |err| {
        break :execution_block beam.raise_exception(err, .{}).v;
    };

    break :execution_block beam.make(#{in_out_var}, .{#{return_opts(return)}}).v;
    """
  end

  def render_return(%{type: :void}),
    do: "_ = result; break :execution_block beam.make(.ok, .{}).v;"

  def render_return(return),
    do: "break :execution_block beam.make(result, .{#{return_opts(return)}}).v;"

  defp return_opts(return) do
    [&return_as/1, &return_length/1]
    |> Enum.flat_map(&List.wrap(&1.(return)))
    |> Enum.join(",")
  end

  defp return_as(%{as: as}), do: ".as = #{render_return_as(as)}"

  defp render_return_as(atom) when is_atom(atom), do: ".#{atom}"
  defp render_return_as({:list, return}), do: ".{.list = #{render_return_as(return)}}"

  defp render_return_as({:map, map_kv_list}),
    do: ".{.map = .{#{render_map_kv_list(map_kv_list)}}}"

  defp render_map_kv_list(map_kv_list) do
    Enum.map_join(map_kv_list, ", ", fn {key, value} ->
      ".#{key} = #{render_return_as(value)}"
    end)
  end

  defp return_length(%{length: length}) when is_integer(length), do: ".length = #{length}"

  defp return_length(%{length: {:arg, arg}}) do
    ~s/.length = (beam.get(usize, .{.v = args[#{arg}]}, .{}) catch {return beam.raise_exception(.invalid_return_length, .{}).v;})/
  end

  defp return_length(_), do: nil
end
