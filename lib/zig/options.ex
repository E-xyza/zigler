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

  def normalize_atom_or_atomlist(opts, key) do
    Keyword.update(opts, key, [], fn atom_or_atomlist ->
      atom_or_atomlist
      |> List.wrap()
      |> Enum.map(&atom_or_raise(&1, opts, key))
    end)
  end

  defp atom_or_raise(atom, _opts, _key) when is_atom(atom), do: atom

  defp atom_or_raise(other, opts, key),
    do: raise_with("option `#{key}` must be a list of atoms", other, opts)

  def validate(opts, keypath, members) when is_list(members) do
    do_validate(
      opts,
      keypath,
      &(&1 in members),
      make_message(keypath, "must be one of #{list_of(members)}")
    )

    opts
  end

  def validate(opts, keypath, :boolean) do
    do_validate(opts, keypath, &is_boolean/1, make_message(keypath, "must be a boolean"))
    opts
  end

  defp do_validate(opts, keypath, fun, message),
    do: do_validate(opts, opts, List.wrap(keypath), fun, message)

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

  defp make_message(keypath, stem) do
    keypath_str =
      keypath
      |> List.wrap()
      |> Enum.map_join(" > ", &to_string/1)

    "option `#{keypath_str}` #{stem}"
  end

  defp list_of(members) do
    Enum.map_join(members, ", ", &"`:#{&1}`")
  end
end
