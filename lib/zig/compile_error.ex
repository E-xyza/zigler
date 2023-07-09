defmodule Zig.CompileError do
  defexception [:command, :code, :error]

  def message(error) do
    "zig command failed: #{error.command} failed with error #{error.code}: #{error.error}"
  end

  alias Zig.Manifest

  def to_error(error, opts) do
    [location_info, error_msg] =
      error.error
      |> String.split("\nerror:")
      |> List.first()
      |> String.split(": error: ")

    manifest = Keyword.fetch!(opts, :manifest)

    [file, line, _column] = String.split(location_info, ":")

    {resolved_file, resolved_line} = Manifest.resolve(manifest, file, String.to_integer(line))

    %CompileError{description: error_msg, file: resolved_file, line: resolved_line}
  end
end
