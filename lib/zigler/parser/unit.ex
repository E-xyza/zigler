defmodule Zigler.Parser.Unit do
  @moduledoc """
  parses zig code and converts test blocks to test functions
  """

  defstruct tests: [], file: nil, offset: 0

  import NimbleParsec

  whitespace = ascii_string([?\s, ?\n], min: 1)
  blankspace = ascii_string([?\s], min: 1)

  @close_paren 41

  defcombinatorp :parenthetical,
    string("(")
    |> repeat(choice([
      parsec(:parenthetical),
      ascii_char(not: @close_paren)]))
    |> string(")")

  init_context = empty()
  |> post_traverse(:init_context)

  # file and line number declaration
  file_line_decl = string("// ref: ")
  |> ascii_string([not: ?\s], min: 1)
  |> string(" line: ")
  |> ascii_string([?0..?9], min: 1)
  |> string("\n")
  |> post_traverse(:parse_file_line_decl)

  # test declaration
  test_decl = ignore(
    string("test")
    |> concat(whitespace)
    |> string("\""))
  |> ascii_string([not: ?\"], min: 1)
  |> ignore(string("\""))
  |> post_traverse(:parse_test_decl)

  # assert statement
  assert_parser = ignore(string("assert("))
  |> repeat(choice([
    parsec(:parenthetical),
    ascii_char(not: @close_paren)]))
  |> ignore(string(")"))
  |> pre_traverse(:parse_assert)

  if Mix.env == :test do
    defparsec :test_parenthetical, parsec(:parenthetical)
    defparsec :test_decl_parser, concat(init_context, test_decl)
    defparsec :test_assert_parser, concat(init_context, assert_parser)
  end

  test_line = optional(blankspace)
  |> concat(test_decl)
  |> concat(whitespace)
  |> string("{")
  |> optional(ascii_string([not: ?\n], min: 1))
  |> string("\n")

  normal_line = optional(
    repeat(choice([
      assert_parser,
      ascii_char(not: ?\n)
    ])))
  |> string("\n")

  unit_code = init_context
  |> repeat(choice([
    file_line_decl,
    test_line,
    normal_line]))

  defparsec :unit_parser, unit_code

  #############################################################################
  ## POST-TRAVERSAL IMPLEMENTATIONS

  # ninjas in a context module for the context map.
  defp init_context(_, _, c = %{file: _}, _, _) do
    {[], struct(__MODULE__, c)}
  end

  defp parse_file_line_decl(_rest, l = [_, line_str | _], context, {line, _char}, _offset) do
    this_line = String.to_integer(line_str)
    {l, %{context | offset: this_line - line + 1}}
  end

  defp parse_test_decl(_rest, [test_name], context, _line, _offset) do
    test_nif = new_nif(test_name)
    {["fn #{test_nif.name}() !void"],
      %{context | tests: [test_nif | context.tests]}}
  end

  defp parse_assert(_rest, content, context, {line, _char}, _offset) do
    test_content = content |> Enum.reverse |> IO.iodata_to_binary
    rewritten = "try assert(#{test_content}, \"#{context.file}\", #{line + context.offset})"
    {[rewritten], context}
  end

  alias Zigler.Parser.Nif
  defp new_nif(name) do
    %Nif{
      name: String.to_atom("test_#{Zigler.Unit.name_to_hash name}"),
      test: String.to_atom(name),
      arity: 0,
      args: [],
      retval: "!void"
    }
  end

end
