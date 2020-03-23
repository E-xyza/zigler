defmodule ZiglerTest.Parser.ImportTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser.Imports

  describe "the import const parser" do
    test "parses a valid import const" do
      assert {:ok, ["foo"], _, _, _, _} =
        Imports.parse_import_const("const foo = @import(\"foo.zig\");\n")
    end
  end

  describe "the import line parser" do
    test "parses a valid import line" do
      assert {:ok, _, _, %{imports: [foo: "foo.zig"]}, _, _} = Imports.parse_import_stmt("const foo = @import(\"foo.zig\");\n")
    end
    test "parses a valid usingnamespace" do
      assert {:ok, _, _, %{imports: [usingnamespace: "foo.zig"]}, _, _} = Imports.parse_import_stmt("usingnamespace @import(\"foo.zig\");\n")
    end
    test "ignores everything else" do
      assert {:error, _, _, _, _, _} = Imports.parse_import_stmt("fn foo(x: i64) i64 {\n")
    end
  end
end
