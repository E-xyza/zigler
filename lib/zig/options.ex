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

  def raise_with(message, content \\ nil, opts) do
    message = if content, do: "#{message}, got: `#{inspect(content)}`", else: message

    raise CompileError,
      description: message,
      file: opts[:file],
      line: opts[:line]
  end
end
