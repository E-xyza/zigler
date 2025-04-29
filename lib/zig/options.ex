defmodule Zig.Options do
  @moduledoc false

  # this module provides common mechanisms for parsing options.

  @type context :: %{
          module: module(),
          file: Path.t(),
          line: pos_integer(),
          keystack: [atom()],
          cleanup: bool
        }

  def initialize_context(caller) do
    caller
    |> Map.take(~w[module file line]a)
    |> Map.merge(%{keystack: [], cleanup: true})
  end

  def push_key(context, key) when is_atom(key), do: Map.update!(context, :keystack, &[key | &1])

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

  def normalize_list(opts, key, callback, type_str, context) when is_function(callback, 2) do
    context = push_key(context, key)

    Keyword.update(opts, key, [], fn
      list when is_list(list) ->
        Enum.map(list, &callback.(&1, context))

      other ->
        raise_with("must be a list of #{type_str}", other, context)
    end)
  end

  def normalize_path(opts, key, context) do
    Keyword.update(opts, key, nil, &IO.iodata_to_binary/1)
  rescue
    _ in ArgumentError ->
      raise_with("`#{key}` option must be a path", opts[key], context)
  end

  def normalize_module(opts, key, keystack \\ [], or_true \\ nil) do
    Keyword.update(opts, key, nil, fn
      module when is_atom(module) ->
        module

      other ->
        or_true_msg = if or_true == :or_true, do: " or `true`"
        raise_with(make_message([key | keystack], "must be a module#{or_true_msg}"), other, opts)
    end)
  end

  def normalize_atom_or_atomlist(opts, key, context) do
    Keyword.update(opts, key, [], fn atom_or_atomlist ->
      atom_or_atomlist
      |> List.wrap()
      |> Enum.map(&atom_or_raise(&1, push_key(context, key)))
    end)
  end

  defp atom_or_raise(atom, _context) when is_atom(atom), do: atom

  defp atom_or_raise(other, context),
    do: raise_with("must be a list of atoms", other, context)

  def normalize_atom(opts, key, keystack) do
    Keyword.update(opts, key, nil, fn
      atom when is_atom(atom) ->
        atom

      other ->
        raise_with(make_message([key | keystack], "must be an atom"), other, opts)
    end)
  end

  def normalize_arity(opts, key, keystack) do
    Keyword.update(opts, key, [], fn arity ->
      arity
      |> List.wrap()
      |> Enum.flat_map(fn
        n when is_integer(n) and n >= 0 ->
          [n]

        a.._//1 = range when a >= 0 ->
          Enum.to_list(range)

        other ->
          raise_with(
            make_message(
              [key | keystack],
              "must be a non-negative integer or range or a list of those"
            ),
            other,
            opts
          )
      end)
    end)
  end

  def normalize_boolean(opts, key, keystack \\ [], overrides \\ []) do
    Enum.map(opts, fn
      {^key, value} when is_boolean(value) ->
        {key, value}

      {^key, other} ->
        raise_with(make_message([key | keystack], "must be a boolean"), other, opts)

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

  def normalize_lookup(opts, key, keystack \\ [], lookup) do
    Enum.map(opts, fn
      {^key, value} when is_map_key(lookup, value) ->
        {key, lookup[value]}

      {^key, other} ->
        raise_with(
          make_message([key | keystack], "must be one of #{list_of(Map.keys(lookup))}"),
          other,
          opts
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

  def validate(opts, key, validation, context)

  def validate(opts, key, members, context) when is_list(members) do
    do_validate(opts, key, &(&1 in members), "must be one of #{list_of(members)}", context)
  end

  def validate(opts, key, :boolean, context) do
    do_validate(opts, key, &is_boolean/1, "must be a boolean", context)
  end

  def validate(opts, key, :atom, context) do
    do_validate(opts, key, &is_atom/1, "must be an atom", context)
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
