defmodule Zigler.Parser.Imports do
  @moduledoc false

  # for parsing zig "import" statements.

  import NimbleParsec

  defstruct imports: [], identifier: nil, pub: false

  @typep identifier_t :: :usingnamespace | String.t
  @type t :: %__MODULE__{
    imports: [{identifier_t, Path.t} | {:pub, identifier_t, Path.t}],
    identifier: atom,
    pub: boolean
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

  import_const = optional(
    string("pub")
    |> concat(ignore(whitespace)))
  |> ignore(
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

  include_stmt =
    ignore(
      string("@cInclude")
      |> optional(whitespace)
      |> string("(")
      |> optional(whitespace)
      |> string("\""))
    |> concat(filename)
    |> ignore(
      string("\"")
      |> optional(whitespace)
      |> string(")"))
    |> post_traverse(:register_include)

  defp register_identifier(_rest, ["usingnamespace" | rest], context, _, _) do
    {[], %{context | identifier: :usingnamespace, pub: pub?(rest)}}
  end
  defp register_identifier(_rest, [identifier | rest], context, _, _) do
    {[], %{context | identifier: identifier, pub: pub?(rest)}}
  end

  defp pub?([]), do: false
  defp pub?(["pub"]), do: true

  defp register_import(_rest, [path], context, _, _) do
    {[],
    %{context |
      imports: [make_identifier(path, context) | context.imports],
      identifier: nil,
      pub: false}}
  end

  defp make_identifier(path, context) do
    if context.pub do
      {:pub, context.identifier, path}
    else
      {context.identifier, path}
    end
  end

  defp register_include(_rest, [path], context, _, _) do
    {[], %{context | imports: [{:cinclude, path} | context.imports]}}
  end

  if Mix.env == :test do
    defparsec :parse_import_const, concat(initialize, import_const)
    defparsec :parse_import_stmt, concat(initialize, import_stmt)
    defparsec :parse_include_stmt, concat(initialize, include_stmt)
  end

  parse_imports =
    initialize
    |> repeat(choice([
      import_stmt,
      include_stmt,
      ascii_char([0..255])
    ]))

  defparsec :parse_imports, parse_imports

  def parse(code) do
    {:ok, _, _, %Zigler.Parser.Imports{imports: imports}, _, _} = parse_imports(code)
    imports
  end

end
