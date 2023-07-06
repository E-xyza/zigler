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


    file = Keyword.fetch!(opts, :file)
    manifest = Keyword.fetch!(opts, :manifest)

    [_file, line, _column] = String.split(location_info, ":")

    resolved_line = Manifest.resolve(manifest, String.to_integer(line))

    %CompileError{description: error_msg, file: file, line: resolved_line}
  end
end
