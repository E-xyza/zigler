defmodule Zig.Manifest do
  # TODO: spec this better
  @type t :: term

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

      defp __resolve(%{file_name: file, line: line}), do: __resolve(file, line)

      defp __resolve(unquote(file), line),
        do: __resolve(@__zig_manifest, [], line)

      defp __resolve(file, line) when is_binary(file), do: {file, line}

      defp __resolve([{anchor_line, _} = head | rest], stack, line) when anchor_line < line do
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

  def create(code) do
    code
    |> String.split("\n")
    |> Enum.with_index(1)
    |> Enum.flat_map(fn
      {"// ref " <> rest, anchor} ->
        [file, line] = String.split(rest, ":")

        [{anchor, {Path.absname(file), String.to_integer(line)}}]

      _ ->
        []
    end)
  end

  def resolve(manifest, file, line) when is_integer(line) do
    resolve(manifest, [], file, line)
  end

  defp resolve([{lesser_line, _} = head | rest], stack, file, line) when lesser_line < line do
    resolve(rest, [head | stack], file, line)
  end

  defp resolve([{bigger_line, _} | _], stack, file, line) when bigger_line >= line do
    case List.first(stack) do
      {anchor, {new_file, relative_line}} ->
        {Path.relative_to_cwd(new_file), line - anchor + relative_line}

      _ ->
        {file, line}
    end
  end

  defp resolve([], stack, file, line) do
    case List.first(stack) do
      {anchor, {new_file, relative_line}} ->
        {Path.relative_to_cwd(new_file), line - anchor + relative_line}

      _ ->
        {file, line}
    end
  end
end
