defmodule Zigler.Parser.Imports do
  @moduledoc false

  # for parsing zig "import" statements.

  import NimbleParsec

  defstruct imports: [], identifier: nil
  @type t :: %__MODULE__{
    imports: [Path.t],
    identifier: atom
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
  identifier = ascii_char([?a..?z, ?A..?Z, ?_])
  |> optional(ascii_string([?a..?z, ?A..?Z, ?0..?9, ?_], min: 1))
  |> reduce({IO, :iodata_to_binary, []})

  usingnamespace = string("usingnamespace")
  |> ignore(whitespace)

  import_const = ignore(
    string("const")
    |> concat(whitespace))
  |> concat(identifier)
  |> ignore(
    optional(whitespace)
    |> string("=")
    |> optional(whitespace))

  prefix = choice(
    [usingnamespace, import_const])
  |> post_traverse(:register_identifier)

  import_stmt =
    prefix
    |> ignore(
      string("@import")
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

  defp register_identifier(_rest, [identifier], context, _, _) do
    {[], %{context | identifier: String.to_atom(identifier)}}
  end

  defp register_import(_rest, [path], context, _, _) do
    {[],
    %{context |
      imports: [{context.identifier, path} | context.imports],
      identifier: nil}}
  end

  if Mix.env == :test do
    defparsec :parse_import_const, concat(initialize, import_const)
    defparsec :parse_import_stmt, concat(initialize, import_stmt)
  end

  parse_imports =
    initialize
    |> repeat(choice([
      import_stmt,
      ascii_char([0..255])
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
