defmodule Zig.CompileError do
  defexception [:command, :code, :error]

  def message(error) do
    "zig command failed: #{error.command} failed with error #{error.code}: #{error.error}"
  end

  def resolve(error, %{module_code_path: module_code_path, manifest_module: manifest_module}) do
    {lines, file_line} =
      error.error
      |> String.split("\n")
      |> Enum.reduce({[], nil}, fn
        error_line, {so_far, nil} ->
          revise_line(error_line, so_far, module_code_path, manifest_module)

        error_line, {so_far, fileline} ->
          {next, _} = revise_line(error_line, so_far, module_code_path, manifest_module)
          {next, fileline}
      end)

    error =
      lines
      |> Enum.reverse()
      |> Enum.join("\n")

    case file_line do
      nil ->
        %CompileError{description: error}

      {file, line} ->
        %CompileError{description: error, file: file, line: line}
    end
  end

  defp revise_line(error_line, acc, module_code_path, manifest_module) do
    with ^module_code_path <> ":" <> rest <- error_line,
         {line, err_rest} <- Integer.parse(rest) do
      {updated_file, updated_line} = manifest_module.__resolve(module_code_path, line)
      {["#{updated_file}:#{updated_line}#{err_rest}" | acc], {updated_file, updated_line}}
    else
      _ -> {[error_line | acc], nil}
    end
  end
end
