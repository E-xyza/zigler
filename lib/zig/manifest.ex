defmodule Zig.Manifest do
  defmacro resolver(manifest, file \\ nil) do
    # TODO: input the file as passed parameter in case we're putting it from a different place.
    file =
      if file do
        file
      else
        __CALLER__.file
        |> Path.dirname()
        |> Path.absname()
        |> Path.join(".#{__CALLER__.module}.zig")
      end

    quote do
      @__zig_manifest unquote(manifest)

      # note that this resolver has to be rebuilt here, so that zigler can be compile-time only.

      defp __resolve(unquote(file), line) do
        __resolve(@__zig_manifest, [], line)
      end

      defp __resolve(file, line) when is_binary(file) do
        {file, line}
      end

      defp __resolve([head = {anchor_line, _} | rest], stack, line) when anchor_line < line do
        __resolve(rest, [head | stack], line)
      end

      defp __resolve([{anchor_line, {file, _}} | _], stack, line) when anchor_line >= line do
        case List.first(stack) do
          {anchor, {file, relative_line}} ->
            {file, line - anchor + relative_line}

          _ ->
            {unquote(file), line}
        end
      end

      defp __resolve([], stack, line) do
        case List.first(stack) do
          {anchor, {file, relative_line}} ->
            {file, line - anchor + relative_line}

          _ ->
            {unquote(file), line}
        end
      end
    end
  end

  def make_manifest(code) do
    code
    |> String.split("\n")
    |> Enum.with_index(fn
      "// ref " <> file_line, index ->
        [file, line] = String.split(file_line, ":")
        # note that line numbers are actually one-indexed.
        [{index + 1, {Path.absname(file), String.to_integer(line)}}]

      _, _ ->
        []
    end)
    |> Enum.flat_map(& &1)
  end

  def resolve(manifest, line) do
    resolve(manifest, [], line)
  end

  defp resolve([head = {lesser_line, _} | rest], stack, line) when lesser_line < line do
    resolve(rest, [head | stack], line)
  end

  defp resolve([{bigger_line, _} | _], stack, line) when bigger_line >= line do
    case List.first(stack) do
      {anchor, {_, relative_line}} ->
        line - anchor + relative_line

      _ ->
        line
    end
  end

  defp resolve([], stack, line) do
    case List.first(stack) do
      {anchor, {_, relative_line}} ->
        line - anchor + relative_line

      _ ->
        line
    end
  end
end
