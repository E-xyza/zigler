defmodule Zig.Options do
  @moduledoc false

  # this module provides common mechanisms for parsing options.

  @type context :: %{
          module: module(),
          file: Path.t(),
          line: pos_integer(),
          keystack: [atom()],
          cleanup: bool,
          otp_app: atom
        }

  def initialize_context(caller, otp_app) do
    caller
    |> Map.take(~w[module file line]a)
    |> Map.merge(%{keystack: [], cleanup: true, otp_app: otp_app})
  end

  def push_key(context, key) when is_atom(key) or is_binary(key),
    do: Map.update!(context, :keystack, &[key | &1])

  def push_key(context, keys) when is_list(keys),
    do: Map.update!(context, :keystack, &Enum.reverse(keys, &1))

  def normalize_as_struct(opts, key, {:int_map, module}, context) do
    Keyword.update(opts, key, %{}, fn
      map when is_map(map) ->
        Map.new(map, fn
          {index, v} when is_integer(index) and index >= 0 ->
            {index, module.new(v, push_key(context, [index, key]))}

          {k, _} ->
            raise_with("must be a map with non-negative integer keys", k, push_key(context, key))
        end)

      other ->
        raise_with("must be a map", other, push_key(context, key))
    end)
  end

  def normalize_as_struct(opts, key, module, context) do
    context = push_key(context, key)
    Keyword.update(opts, key, module.new([], context), &module.new(&1, context))
  end

  def normalize(opts, key, default \\ nil, callback, context) do
    Keyword.update(opts, key, default, &callback.(&1, push_key(context, key)))
  end

  def normalize_path(opts, key, context) do
    Keyword.update(opts, key, nil, &IO.iodata_to_binary/1)
  rescue
    _ in ArgumentError ->
      raise_with("`#{key}` option must be a path", opts[key], context)
  end

  def normalize_boolean(opts, key, context, overrides \\ []) do
    Enum.map(opts, fn
      {^key, value} when is_boolean(value) ->
        {key, value}

      {^key, other} ->
        raise_with("must be boolean", other, push_key(context, key))

      maybe_override when is_atom(maybe_override) ->
        case Keyword.fetch(overrides, maybe_override) do
          {:ok, value} when is_boolean(value) ->
            {key, value}

          _ ->
            maybe_override
        end

      other ->
        other
    end)
  end

  # TODO: delete this function
  def normalize_lookup(opts, key, lookup, context) do
    Enum.map(opts, fn
      {^key, value} when is_map_key(lookup, value) ->
        {key, lookup[value]}

      {^key, other} ->
        raise_with(
          "must be one of #{list_of(Map.keys(lookup))}",
          other,
          push_key(context, key)
        )

      maybe_override when is_atom(maybe_override) ->
        case Map.fetch(lookup, maybe_override) do
          {:ok, value} ->
            {key, value}

          _ ->
            maybe_override
        end

      other ->
        other
    end)
  end

  # TODO: delete this function
  def normalize_pathlist(opts, key, context) do
    Keyword.update(opts, key, [], fn
      [n | _] = charlist when is_integer(n) ->
        [resolve_path("#{charlist}", push_key(context, key))]

      path_or_paths ->
        path_or_paths
        |> List.wrap()
        |> Enum.map(&resolve_path(&1, push_key(context, key)))
    end)
  end

  defp resolve_path({:system, path}, context) do
    {:system, IO.iodata_to_binary(path)}
  rescue
    _ in ArgumentError ->
      raise_with("system path must be iodata", path, context)
  end

  defp resolve_path({:priv, path}, context) do
    path
    |> IO.iodata_to_binary()
    |> Path.expand(:code.priv_dir(context.otp_app))
  rescue
    _ in ArgumentError ->
      raise_with("priv path must be iodata", path, context)
  end

  defp resolve_path(path, context) do
    path
    |> IO.iodata_to_binary()
    |> Path.expand(Path.dirname(context.file))
  rescue
    _ in ArgumentError ->
      raise_with(
        "must be path, `{:priv, path}`, `{:system, path}`, or a list of those",
        path,
        context
      )
  end

  # TODO: delete this function

  def normalize_c_src(opts, key, context) do
    Keyword.update(opts, key, [], fn
      [n | _] = charlist when is_integer(n) ->
        [{resolve_path("#{charlist}", push_key(context, key)), []}]

      src_or_srcs ->
        src_or_srcs
        |> List.wrap()
        |> Enum.flat_map(fn src_def ->
          src_def
          |> resolve_src(push_key(context, key))
          |> expand_wildcards()
        end)
    end)
  end

  defp resolve_src({tag, _} = path, context) when tag in ~w[priv system]a do
    path
    |> resolve_path(context)
    |> normalize_src_options([], context)
  end

  defp resolve_src({tag, path, options}, context) when tag in ~w[priv system]a do
    {tag, path}
    |> resolve_path(context)
    |> normalize_src_options(options, context)
  end

  defp resolve_src({path, options}, context) do
    path
    |> IO.iodata_to_binary()
    |> Path.expand(Path.dirname(context.file))
    |> normalize_src_options(options, context)
  rescue
    _ in ArgumentError ->
      raise_with("source files must be iodata", path, context)
  end

  defp resolve_src(path, context) do
    path
    |> IO.iodata_to_binary()
    |> Path.expand(Path.dirname(context.file))
    |> normalize_src_options([], context)
  rescue
    _ in ArgumentError ->
      raise_with(
        "must be path, `{path, [opts]}`, `{:priv, path}`, `{:priv, path, [opts]}`, `{:system, path}`, `{:system, path, [opts]}`, or a list of those",
        path,
        context
      )
  end

  defp normalize_src_options(path, opts, context) when is_list(opts) do
    {path, Enum.map(opts, &IO.iodata_to_binary/1)}
  rescue
    _ in ArgumentError ->
      raise_with("must be a list of iodata", opts, push_key(context, inspect(path)))
  end

  defp expand_wildcards({{:system, path}, opts} = spec) do
    {path, opts}
    |> expand_wildcards()
    |> Enum.map(fn {path, opts} -> {{:system, path}, opts} end)
  end

  defp expand_wildcards({path, opts} = spec) do
    if String.ends_with?(path, "/*") do
      dir = Path.dirname(path)

      dir
      |> File.ls!()
      |> Enum.map(&{Path.join(dir, &1), opts})
    else
      [spec]
    end
  end

  def scrub_non_keyword(opts, context) do
    Enum.map(opts, fn
      {key, _} = kv when is_atom(key) ->
        kv

      other ->
        raise_with(
          "found an invalid term in the options list",
          other,
          context
        )
    end)
  end

  def validate(opts, key, members, context) when is_list(members) do
    do_validate(opts, key, &(&1 in members), "must be one of #{list_of(members)}", context)
  end

  def validate(opts, key, :boolean, context) do
    do_validate(opts, key, &is_boolean/1, "must be a boolean", context)
  end

  def validate(opts, key, :atom, context) do
    do_validate(opts, key, &is_atom/1, "must be an atom", context)
  end

  def validate(opts, key, {:atom, substitute}, context) do
    do_validate(opts, key, &is_atom/1, "must be #{substitute}", context)
  end

  def validate(opts, key, fun, context) when is_function(fun, 1) do
    do_validate(opts, key, fun, "", context)
  end

  defp do_validate(opts, key, fun, message, context) do
    context = push_key(context, key)

    case fetch_and_run(opts, key, fun) do
      true ->
        opts

      false ->
        raise_with(message, opts[key], context)

      :ok ->
        opts

      {:error, substitute_message} ->
        raise_with(substitute_message, context)

      {:error, substitute_message, content} ->
        raise_with(substitute_message, content, context)
    end
  end

  def fetch_and_run(opts, key, fun) do
    case Keyword.fetch(opts, key) do
      {:ok, value} ->
        fun.(value)

      :error ->
        :ok
    end
  end

  def raise_with(message, content \\ nil, context) do
    message =
      case content do
        {:tag, label, content} ->
          "#{message}, got: `#{inspect(content)}` for #{label}"

        nil ->
          message

        content ->
          "#{message}, got: `#{inspect(content)}`"
      end

    raise CompileError,
      description: make_message(context.keystack, message),
      file: context.file,
      line: context.line
  end

  defp make_message([], stem), do: stem

  defp make_message(keystack, stem) do
    keystack_str =
      keystack
      |> List.wrap()
      |> Enum.reverse()
      |> Enum.map_join(" > ", &to_string/1)

    "option `#{keystack_str}` #{stem}"
  end

  defp list_of(members) do
    Enum.map_join(members, ", ", &"`:#{&1}`")
  end
end
