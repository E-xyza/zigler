defmodule Zigler.Compiler.ErrorParser do

  @moduledoc false

  import NimbleParsec

  @numbers [?0..?9]

  errormsg =
    ignore(repeat(ascii_char([?\s])))
    |> ascii_string([not: ?:], min: 1)
    |> ignore(string(":"))
    |> ascii_string(@numbers, min: 1)
    |> ignore(string(":"))
    |> ascii_string(@numbers, min: 1)
    |> ignore(string(":"))
    |> ascii_string([not: ?\n], min: 1)
    |> ignore(string("\n"))

  defparsec :parse_error, times(errormsg, min: 1)

  @spec backreference(Path.t, non_neg_integer) :: {Path.t, non_neg_integer}
  @doc """
  given a code file path and a line number, calculates the file and line number
  of the source document from which it came.  Strongly depends on having
  fencing comments of the form `// <file> line: <line>` in order to backtrack
  this information.
  """
  def backreference(path, str_line) do
    line = String.to_integer(str_line)
    file_content = File.read!(path)

    {offset, true_file, original_line} = file_content
    |> parse_line_comments
    |> Enum.filter(fn {a, _, _} -> a <= line end)
    |> List.last

    true_line = line - offset + original_line + 1
    {true_file, true_line}
  end

  @doc """
  given a zig compiler error message, a directory for the code file, and the temporary
  directory where code assembly is taking place, return an appropriate `CompileError`
  struct which can be raised to emit a sensible compiler error message.

  The temporary directory is stripped (when reasonable) in favor of a "true" source file
  and any filename substitutions are performed as well.
  """
  def parse(msg, code_dir, tmp_dir) do
    {:ok, [path, line, _col, msg | _rest], _, _, _, _} = parse_error(msg)
    #TODO: figure out if we want to do something with "other information lines" later.

    # check if we need full translation or just file translation.
    if Path.basename(path) == "zig_nif.zig" do
      # read the actual path file
      {true_file, true_line} = backreference(path, line)

      %CompileError{
        file: true_file,
        line: true_line,
        description: String.trim(msg)
      }
    else
      rel_path = Path.relative_to(path, tmp_dir)
      %CompileError{
        file: Path.join(code_dir, rel_path),
        line: String.to_integer(line),
        description: String.trim(msg)
      }
    end
  end

  line_comment =
    repeat(ascii_char([0..255]) |> lookahead_not(string("//")))
    |> ascii_char([0..255])
    |> string("// ")
    |> concat(ascii_string([not: ?\s], min: 1) |> tag(:file))
    |> string(" line: ")
    |> concat(ascii_string(@numbers, min: 1) |> tag(:line))
    |> repeat(ascii_char([?\s]))
    |> string("\n")
    |> post_traverse(:parse_group)

  if Mix.env == :test do
    # TODO: tests on parameters
    defparsec :parse_line_comment, line_comment
  end

  line =
    utf8_string([not: ?\n], min: 1)
    |> string("\n")

  empty_line = string("\n")

  by_line =
    repeat(choice([
      line_comment,
      line,
      empty_line
    ]))

  defparsec :by_line_comments, by_line

  defp parse_group(_rest, content, context, {ctx_line, _}, _) do
    kw = Enum.flat_map(content, fn
      {:file, [file]} -> [{:file, file}]
      {:line, [line]} -> [{:line, String.to_integer(line)}]
      _ -> []
    end)
    {[{:group, {ctx_line, kw[:file], kw[:line]}}], context}
  end

  # read our instrumented comments to try to find the location of the error.
  defp parse_line_comments(txt) do
    {:ok, parsed, _, _, _, _} = by_line_comments(txt)

    parsed
    |> Enum.flat_map(fn
      {:group, data} -> [data]
      _ -> []
    end)
  end


end
