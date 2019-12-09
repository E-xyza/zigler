defmodule Zigler.Import do

  @moduledoc false

  import NimbleParsec

  whitespace = ascii_string([?\s, ?\n], min: 1)

  @alphanumeric [?a..?z, ?A..?Z, ?0..?9, ?_]

  import_line =
    repeat(ascii_char([?\s]))
    |> optional(string("pub") |> repeat(ascii_char([?\s])))
    |> optional(string("const") |> repeat(ascii_char([?\s])))
    |> ascii_string(@alphanumeric, min: 1)
    |> optional(whitespace)
    |> string("=")
    |> optional(whitespace)
    |> string("@import(\"")
    |> concat(ascii_string([not: ?"], min: 1) |> tag(:import))
    |> string("\");")
    |> repeat(ascii_char([?\s]))
    |> choice([string("\n"), eos()])

  line =
    utf8_string([not: ?\n], min: 1)
    |> string("\n")

  empty_line = string("\n")

  by_line =
    repeat(choice([
      import_line,
      line,
      empty_line
    ]))

  defparsec :imports_by_line, by_line

  def find(code) do
    {:ok, content, _, _, _, _} = imports_by_line(code)

    Enum.flat_map(content, fn
      {:import, [path]} -> [path]
      _ -> []
    end)
  end

  def recursive_find(code, root) do
    recursive_find(find(code), [], root)
  end
  def recursive_find([file | rest], files_so_far, root) do
    new_files = root
    |> Path.join(file)
    |> File.read!
    |> find

    # re-home the file to the new directory.
    rehomed_files = Enum.map(new_files, fn new_file ->
      file
      |> Path.dirname
      |> Path.join(new_file)
    end)

    append_files = rehomed_files -- files_so_far

    recursive_find(rest ++ append_files, [file | files_so_far], root)
  end
  def recursive_find([], files_so_far, _), do: files_so_far

end
