defmodule Zig.Options do
  @moduledoc false

  # this module provides common mechanisms for parsing options.

  def normalize_as_struct(opts, key, module) do
    Keyword.update(opts, key, module.new([], opts), &module.new(&1, opts))
  end

  def normalize_list(opts, key, callback, type) when is_function(callback, 1) do
    Keyword.update(opts, key, [], fn
      list when is_list(list) ->
        Enum.map(list, callback)

      other ->
        raise_with("`#{key}` option must be a list of #{type}", other, opts)
    end)
  end

  def normalize_path(opts, key) do
    Keyword.update(opts, key, nil, &IO.iodata_to_binary/1)
  rescue
    _ in ArgumentError ->
      raise_with("`#{key}` option must be a path", opts[key], opts)
  end

  def normalize_module(opts, [key | _] = keystack, or_true \\ nil) do
    Keyword.update(opts, key, nil, fn
      module when is_atom(module) ->
        module

      other ->
        or_true_msg = if or_true == :or_true, do: " or `true`"
        raise_with(make_message(keystack, "must be a module#{or_true_msg}"), other, opts)
    end)
  end

  def normalize_atom_or_atomlist(opts, key) do
    Keyword.update(opts, key, [], fn atom_or_atomlist ->
      atom_or_atomlist
      |> List.wrap()
      |> Enum.map(&atom_or_raise(&1, opts, key))
    end)
  end

  def normalize_boolean(opts, [key | _] = keystack, overrides \\ []) do
    Enum.map(opts, fn
      {^key, value} when is_boolean(value) ->
        {key, value}

      {^key, other} ->
        raise_with(make_message(keystack, "must be a boolean"), other, opts)

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

  def normalize_lookup(opts, [key | _] = keystack, lookup) do
    Enum.map(opts, fn 
      {^key, value} when is_map_key(lookup, value) ->
        {key, lookup[value]}
      {^key, other} ->
        raise_with(make_message(keystack, "must be one of #{list_of(Map.keys(lookup))}"), other, opts)
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

  defp atom_or_raise(atom, _opts, _key) when is_atom(atom), do: atom

  defp atom_or_raise(other, opts, key),
    do: raise_with("option `#{key}` must be a list of atoms", other, opts)

  def validate(opts, keystack, members) when is_list(members) do
    do_validate(
      opts,
      keystack,
      &(&1 in members),
      make_message(keystack, "must be one of #{list_of(members)}")
    )

    opts
  end

  def validate(opts, keystack, :boolean) do
    do_validate(opts, keystack, &is_boolean/1, make_message(keystack, "must be a boolean"))
    opts
  end

  defp do_validate(opts, keystack, fun, message),
    do: do_validate(opts, opts, List.wrap(keystack), fun, message)

  defp do_validate(parent_opts, opts, [head | rest], fun, message) do
    case Keyword.fetch(opts, head) do
      {:ok, value} ->
        do_validate(parent_opts, value, rest, fun, message)

      :error ->
        :ok
    end
  end

  defp do_validate(parent_opts, value, [], fun, message) do
    if fun.(value) do
      :ok
    else
      raise_with(message, value, parent_opts)
    end
  end

  defp do_validate(parent_opts, value, key, fun, message) when is_atom(key) do
    do_validate(parent_opts, value, [key], fun, message)
  end

  def raise_with(message, content \\ nil, opts) do
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
      description: message,
      file: opts[:file],
      line: opts[:line]
  end

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
