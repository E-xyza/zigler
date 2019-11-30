defmodule ZiglerTest.UnitParserTest do
  use ExUnit.Case, async: true

  alias Zigler.Unit.Parser
  alias Zigler.Unit.Import

  describe "the test header parser" do
    test "can identify test headers" do
      code = """
        test "this is a test" {
          assert 1 == 1;
        }
      """

      assert {:ok, content, _, _, _, _} = Parser.parse_test_header(code)
    end
  end

  describe "the identifier parser" do
    test "can correctly identify identifiers" do
      assert {:ok, ["abcdef_2"], _, _, _, _} = Parser.parse_identifier("abcdef_2")
      assert {:ok, ["__test_me__"], _, _, _, _} = Parser.parse_identifier("__test_me__")
    end

    test "fails on non_identifiers" do
      assert {:error, _, _, _, _, _} = Parser.parse_identifier("2abce")
    end

    test "fails on strange symbols" do
      assert {:error, _, _, _, _, _} = Parser.parse_identifier("!abce")
    end
  end

  describe "the import parser" do
    test "can identify import statements" do
      code = """
      const abc = @import("my_file.zig");
      """

      assert {:ok, [%Import{}, "pub const abc = @import(\"my_file.zig\");" <> _], _, _, _, _} = Parser.parse_imports(code)
    end

    test "leaves pub imports alone" do
      code = """
      pub const abc = @import("my_file.zig");
      """

      assert {:ok, [%Import{}, "pub const abc = @import(\"my_file.zig\");" <> _], _, _, _, _} = Parser.parse_imports(code)
    end
  end
end
