defmodule Zig.CompileError do
  defexception [:command, :code, :error]

  alias Zig.Command

  def message(error) do
    "zig command failed: #{error.command} failed with error #{error.code}: #{error.error}"
  end

  def resolve(error, %{zig_code_path: zig_code_path, manifest_module: manifest_module}) do
    {lines, file_line} =
      error.error
      |> Command.split_on_newline()
      |> Enum.reduce({[], nil}, fn
        error_line, {so_far, nil} ->
          {maybe_line, fileline} = revise_line(error_line, so_far, zig_code_path, manifest_module)

          case List.flatten(maybe_line) do
            [str1, str2 | rest] when is_binary(str1) and is_binary(str2) ->
              {[IO.iodata_to_binary(rest)], fileline}

            _ ->
              {maybe_line, fileline}
          end

        error_line, {so_far, fileline} ->
          {next, _} = revise_line(error_line, so_far, zig_code_path, manifest_module)
          {next, fileline}
      end)

    error =
      lines
      |> Enum.reverse()
      |> IO.iodata_to_binary()
      |> String.trim()

    case file_line do
      nil ->
        %CompileError{description: error}

      {file, line} ->
        %CompileError{description: error, file: file, line: line}
    end
  end

  defp revise_line(error_line, acc, absolute_path, manifest_module) do
    relative_path = Path.relative_to_cwd(absolute_path)

    {replaced_line, fileline} =
      parse_line(error_line, absolute_path, relative_path, manifest_module)

    {[replaced_line, Command.newline() | acc], fileline}
  end

  defp parse_line(error_line, absolute_path, relative_path, manifest_module) do
    absolute_path = adjust_windows_path(absolute_path)

    cond do
      # if it's the first part of the error message, we must memoize the new file/line
      String.starts_with?(error_line, "#{absolute_path}:") ->
        linerest = String.trim_leading(error_line, "#{absolute_path}:")
        {linerest, absolute_path, manifest_module}

        {new_file, new_line, rest} =
          do_resolution(linerest, absolute_path, manifest_module)

        {[
           new_file,
           colonline(new_line),
           rest
           |> just_replace(absolute_path, relative_path, manifest_module)
           |> remove_column()
         ], {new_file, new_line}}

      String.starts_with?(error_line, "#{relative_path}:") ->
        linerest = String.trim_leading(error_line, "#{relative_path}:")
        {new_file, new_line, rest} = do_resolution(linerest, absolute_path, manifest_module)

        {[
           new_file,
           colonline(new_line),
           rest
           |> just_replace(absolute_path, relative_path, manifest_module)
           |> remove_column()
         ], {new_file, new_line}}

      :else ->
        {just_replace(error_line, absolute_path, relative_path, manifest_module), nil}
    end
  end

  if {:win32, :nt} == :os.type() do
    defp adjust_windows_path(path) do
      case String.replace(path, "/", "\\") do
        <<drive_letter, ":", rest::binary>> when drive_letter in ?a..?z ->
          <<drive_letter - 32, ":", rest::binary>>

        other ->
          other
      end
    end
  else
    defp adjust_windows_path(path), do: path
  end

  defp remove_column(charlist_line) do
    with ~c':' ++ rest <- charlist_line,
         {_, rest} <- Integer.parse("#{rest}"),
         ": " <> rest <- rest do
      rest
    else
      _ -> charlist_line
    end
  end

  defp colonline(line) do
    if line, do: ":#{line}", else: []
  end

  defp do_resolution(linerest, absolute_path, manifest_module) do
    with {lineno, rest} <- Integer.parse(linerest),
         {file, line} <-
           manifest_module.__resolve(%{file_name: absolute_path, line: lineno}) do
      {file, line, rest}
    else
      _ ->
        {file, _} = manifest_module.__resolve(%{file_name: absolute_path, line: 1})
        {file, nil, linerest}
    end
  end

  defp just_replace("", _, _, _), do: []

  defp just_replace(error_rest, absolute_path, relative_path, manifest_module) do
    cond do
      # if it's the first part of the error message, we must memoize the new file/line
      String.starts_with?(error_rest, "#{absolute_path}:") ->
        linerest = String.trim_leading(error_rest, "#{absolute_path}:")
        {new_file, new_line, rest} = do_resolution(linerest, absolute_path, manifest_module)

        [
          new_file,
          colonline(new_line),
          just_replace(rest, absolute_path, relative_path, manifest_module)
        ]

      String.starts_with?(error_rest, "#{relative_path}:") ->
        linerest = String.trim_leading(error_rest, "#{relative_path}:")
        {new_file, new_line, rest} = do_resolution(linerest, absolute_path, manifest_module)

        [
          new_file,
          colonline(new_line),
          just_replace(rest, absolute_path, relative_path, manifest_module)
        ]

      :else ->
        <<one_char, rest::binary>> = error_rest
        [one_char | just_replace(rest, absolute_path, relative_path, manifest_module)]
    end
  end
end
