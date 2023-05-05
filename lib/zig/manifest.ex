defmodule Zig.Manifest do
  defmacro resolver(code, file \\ nil) do
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

    manifest = make_manifest(code)

    quote do
      @__zig_manifest unquote(manifest)

      defp __resolve(unquote(file), line) do
        __resolve(@__zig_manifest, [], line)
      end

      defp __resolve(file, line) when is_binary(file) do
        {file, line}
      end

      defp __resolve([head = {lesser_line, _} | rest], stack, line) when lesser_line < line do
        __resolve(rest, [head | stack], line)
      end

      defp __resolve([{bigger_line, {file, _}} | _], stack, line) when bigger_line >= line do
        case List.first(stack) do
          {anchor, {file, relative_line}} ->
            {file, line - anchor + relative_line + 1}

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

  defp make_manifest(code) do
    code
    |> String.split("\n")
    |> Enum.with_index(fn
      "// ref " <> file_line, index ->
        [file, line] = String.split(file_line, ":")
        # note that line numbers are actually one-indexed.
        [{index, {Path.absname(file), String.to_integer(line)}}]

      _, _ ->
        []
    end)
    |> Enum.flat_map(& &1)
  end
end
