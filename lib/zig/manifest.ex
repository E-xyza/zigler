defmodule Zig.Manifest do
  defmacro elixir_function(file \\ nil) do
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
end
