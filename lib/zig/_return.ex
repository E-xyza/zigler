defmodule Zig.Return do
  @moduledoc false

  defstruct ~w[cleanup type in_out error length spec struct]a ++ [as: :default]

  alias Zig.Options
  alias Zig.Type

  # information supplied by the user. 
  @type unmerged :: %__MODULE__{
          cleanup: boolean,
          error: atom,
          length: nil | non_neg_integer | {:arg, non_neg_integer()},
          spec: nil | Macro.t(),
          struct: nil | module,
          as: Zig.as_type(),
          in_out: nil | String.t()
        }

  # information obtained by semantic analysis.  Cleanup must be present
  # as the cleanup clause is inherited by the module rules cleanup.
  @type sema :: %__MODULE__{
          type: Type.t()
        }

  # type as merged after semantic analysis.
  @type t :: %__MODULE__{
          type: Type.t(),
          cleanup: boolean,
          error: atom,
          length: nil | non_neg_integer | {:arg, non_neg_integer()},
          spec: nil | Macro.t(),
          struct: nil | module,
          as: Zig.as_type(),
          in_out: nil | String.t()
        }

  @spec new(Zig.return_options(), Options.context()) :: unmerged

  def new(context), do: new([], context)

  def new(opts, context) do
    opts
    |> List.wrap()
    |> Options.normalize(:cleanup, Options.boolean_normalizer(noclean: false), context)
    |> Options.normalize(:as, &normalize_type/2, context)
    |> normalize_map_list(context)
    |> Options.scrub_non_keyword(context)
    |> Options.validate(:length, &validate_length/1, context)
    |> Options.validate(:error, {:atom, "a module"}, context)
    |> Options.validate(:struct, :atom, context)
    |> Keyword.put_new(:cleanup, context.cleanup)
    |> then(&struct!(__MODULE__, &1))
  rescue
    e in KeyError ->
      Options.raise_with("was supplied the invalid option `#{e.key}`", context)
  end

  @as ~w[binary list integer map default]a
  @deep ~w[list map]a

  def normalize_type({type}, _context) when type in @as, do: {:ok, type}
  def normalize_type({_}, _context), do: :error
  def normalize_type(type, _context) when type in @as, do: type

  def normalize_type({t, _} = type, context) when t in @deep do
    validate_type(type, context)
    type
  end

  @invalid_type_error "has an invalid type specification (must be `:binary`, `:list`, `:map`, `:default`, or `{:list, type}`, `{:map, key: type}`)"

  def normalize_type(other, context) do
    Options.raise_with(
      @invalid_type_error,
      other,
      context
    )
  end

  def normalize_map_list(opts, context) do
    Enum.map(opts, fn
      {t, _} = type when t in @deep ->
        validate_type(type, context)
        {:as, type}

      other ->
        other
    end)
  end

  @length_error "must be a non-negative integer or `{:arg, argument index}`"
  defp validate_length(length) when is_integer(length) and length >= 0, do: :ok
  defp validate_length({:arg, length}) when is_integer(length) and length >= 0, do: :ok
  defp validate_length(wrong), do: {:error, @length_error, wrong}

  defp validate_type(type, _context) when type in @as, do: type

  defp validate_type({:list, type}, context),
    do: validate_type(type, Options.push_key(context, :list))

  defp validate_type({:map, list}, context),
    do: validate_map_type(list, Options.push_key(context, :map))

  defp validate_type(wrong, context), do: Options.raise_with(@invalid_type_error, wrong, context)

  @invalid_map_type_error "has an invalid map type specification (map parameter must be a keyword list of atoms as keys and type specifications as values)"

  defp validate_map_type(list, context) when is_list(list) do
    Enum.each(list, fn
      {key, type} when is_atom(key) ->
        validate_type(type, Options.push_key(context, key))

      _ ->
        Options.raise_with(@invalid_map_type_error, list, context)
    end)
  end

  defp validate_map_type(wrong, context),
    do: Options.raise_with(@invalid_map_type_error, wrong, context)

  @spec merge(sema, unmerged) :: t
  def merge(sema, spec) do
    Enum.reduce(~w[cleanup in_out error length spec as struct]a, sema, fn key, sema ->
      Map.put(sema, key, Map.fetch!(spec, key))
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
    [&return_as/1, &return_length/1, &return_struct/1]
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

  defp return_struct(%{struct: module}) do
    if module, do: ~s(.@"struct" = .@"#{module}")
  end
end
