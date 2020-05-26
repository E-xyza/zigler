defmodule Zigler.Parser.Error do
  @moduledoc """
  parses errors emitted by the zig compiler
  """

  import NimbleParsec
  require Logger

  @numbers [?0..?9]

  whitespace = ascii_string([?\s, ?\n], min: 1)

  errormsg =
    ignore(repeat(ascii_char([?\s])))
    |> ascii_string([not: ?:], min: 1)
    |> ignore(string(":"))
    |> ascii_string(@numbers, min: 1)
    |> ignore(string(":"))
    |> ascii_string(@numbers, min: 1)
    |> ignore(string(":")
      |> optional(whitespace)
      |> string("error:")
      |> optional(whitespace))
    |> ascii_string([not: ?\n], min: 1)
    |> ignore(string("\n"))

  defparsec :parse_error, times(errormsg, min: 1)

  @doc """
  given a zig compiler error message, a directory for the code file, and the temporary
  directory where code assembly is taking place, return an appropriate `CompileError`
  struct which can be raised to emit a sensible compiler error message.

  The temporary directory is stripped (when reasonable) in favor of a "true" source file
  and any filename substitutions are performed as well.
  """
  def parse(msg, compiler) do
    case parse_error(msg) do
      {:ok, [path, line, _col | msg], rest, _, _, _} ->
        {path, line} = compiler.assembly_dir
        |> Path.join(path)
        |> Path.expand
        |> backreference(String.to_integer(line))

        raise CompileError,
          file: path,
          line: line,
          description: IO.iodata_to_binary([msg, "\n" | rest])
      _ ->
        message = """
        this zig compiler hasn't been incorporated into the parser.
        Please file a report at:
        https://github.com/ityonemo/zigler/issues
        """ <> msg
        raise CompileError,
          description: message
    end
  end

  @spec backreference(Path.t, non_neg_integer) :: {Path.t, non_neg_integer}
  @doc """
  given a code file path and a line number, calculates the file and line number
  of the source document from which it came.  Strongly depends on having
  fencing comments of the form `// ref: <file> line: <line>` in order to backtrack
  this information.
  """
  def backreference(path, line) do
    path
    |> File.stream!
    |> Stream.map(&check_ref/1)
    |> Stream.take(line)
    |> Stream.with_index
    |> Enum.reduce({path, line}, &trap_last_ref(&1, &2, line))
  end

  # initialize the value of the last line to the existing line
  defp trap_last_ref({{:ok, [path, line_number], _, _, _, _}, line_idx}, _, line) do
    {path, line - line_idx + String.to_integer(line_number) - 1}
  end
  defp trap_last_ref(_, prev, _), do: prev

  path = ascii_string([not: ?\s, not: ?\t], min: 1)

  check_ref = ignore(
    string("// ref:")
    |> concat(whitespace))
  |> concat(path)
  |> ignore(
    whitespace
    |> string("line:")
    |> concat(whitespace))
  |> ascii_string(@numbers, min: 1)

  defparsec :check_ref, check_ref
end
