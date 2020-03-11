defmodule Zigler.Parser.Imports do
  @moduledoc false

  # for parsing zig "import" statements.

  import NimbleParsec

  defstruct imports: []
  @type t :: %__MODULE__{
    imports: [Path.t]
  }

  # designed to ninja in this struct as necessary.
  @type line_info :: {non_neg_integer, non_neg_integer}
  @type parsec_retval :: {[String.t], t}
  initialize = post_traverse(empty(), :initializer)
  @spec initializer(String.t, [String.t], t, line_info, non_neg_integer)
    :: parsec_retval
  defp initializer(_, _, context, _, _), do: {[], struct(__MODULE__, context)}

  whitespace = ascii_string([?\s, ?\n], min: 1)
  filename = ascii_string([not: ?"], min: 1)

  import_line =
    ignore(
      repeat(
        lookahead_not(string("@import"))
        |> ascii_char(not: ?\n))
      |> string("@import")
      |> optional(whitespace)
      |> string("(")
      |> optional(whitespace)
      |> string("\""))
    |> concat(filename)
    |> ignore(
      string("\"")
      |> optional(whitespace)
      |> string(")"))
    |> post_traverse(:register_import)

  defp register_import(_rest, [path], context = %{imports: imports}, _, _) do
    {[], %{context | imports: [path | imports]}}
  end

  if Mix.env == :test do
    defparsec :parse_import_line, concat(initialize, import_line)
  end

  ignored_line =
    optional(utf8_string([not: ?\n], min: 1))
    |> string("\n")
    |> post_traverse(:clear)

  parse_imports =
    initialize
    |> repeat(choice([
      import_line,
      ignored_line
    ]))

  defparsec :parse_imports, parse_imports

  @spec clear(String.t, [String.t], t, line_info, non_neg_integer) :: parsec_retval

  defp clear(_rest, _content, context, _, _) do
    {[], context}
  end

  def parse(code) do
    {:ok, _, _, %Zigler.Parser.Imports{imports: imports}, _, _} = parse_imports(code)
    imports
  end

end
