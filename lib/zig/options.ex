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

  # context operations

  def initialize_context(caller, otp_app) do
    caller
    |> Map.take(~w[module file line]a)
    |> Map.merge(%{keystack: [], cleanup: true, otp_app: otp_app})
  end

  def push_key(context, key) when is_atom(key) or is_binary(key) or is_integer(key),
    do: Map.update!(context, :keystack, &[key | &1])

  # normalization and validation

  def normalize_kw(opts, key, default \\ nil, callback, context) do
    Keyword.update(opts, key, default, &callback.(&1, push_key(context, key)))
  end

  def normalize_path(opts, key, context) do
    Keyword.update(opts, key, nil, &Zig._normalize_path(&1, Path.dirname(context.file)))
  rescue
    _ ->
      raise_with("must be a path", opts[key], push_key(context, key))
  end

  def boolean_normalizer([{key, value}]) when is_atom(key) and is_boolean(value),
    do: fn
      {^key}, _context ->
        {:ok, value}

      {_}, _context ->
        :error

      value, _context when is_boolean(value) ->
        value

      other, context ->
        raise_with("must be boolean", other, context)
    end

  def struct_normalizer(module), do: &module.new/2

  def normalize(opts, key, fun, context) do
    Enum.map(opts, fn
      {^key, value} ->
        {key, fun.(value, push_key(context, key))}

      {_other, _} = kv ->
        kv

      atom when is_atom(atom) ->
        case fun.({atom}, push_key(context, key)) do
          {:ok, value} -> {key, value}
          :error -> atom
        end
    end)
  rescue
    _ in FunctionClauseError ->
      raise_with("must be a list of `{atom, term}` or `atom`", opts, context)
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
    case fetch_and_run(opts, key, fun) do
      true ->
        opts

      false ->
        raise_with(message, opts[key], push_key(context, key))

      :ok ->
        opts

      {:error, substitute_message} ->
        raise_with(substitute_message, push_key(context, key))

      {:error, substitute_message, content} ->
        raise_with(substitute_message, content, push_key(context, key))
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

  @spec raise_with(String.t(), term, term) :: no_return
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

  def list_of(members) do
    Enum.map_join(members, ", ", &"`#{inspect(&1)}`")
  end
end
