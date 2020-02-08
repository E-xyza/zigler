defmodule ZiglerTest.CompilerErrorParserTest do
  use ExUnit.Case, async: true

  alias Zigler.Compiler.ErrorParser

  @moduletag :parser

  test "the compiler line parser" do
    line_comment = "fn one() i64 { // /home/ityonemo/code/zigler/test/assets/zig_syntax_error_module.exs line: 13\n"
    assert {:ok, _, _, _, _, _} = ErrorParser.parse_line_comment(line_comment)
  end
end
