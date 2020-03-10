defmodule ZiglerTest.Parser.ImportTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser.Imports

  describe "the import line parser" do
    test "parses a valid import line" do
      assert {:ok, _, _, %{imports: ["foo.zig"]}, _, _} = Imports.parse_import_line("const foo = @import(\"foo.zig\");\n")
    end
    test "parses a valid usingnamespace" do
      assert {:ok, _, _, %{imports: ["foo.zig"]}, _, _} = Imports.parse_import_line("usingnamespace @import(\"foo.zig\");\n")
    end
    test "ignores everything else" do
      assert {:error, _, _, _, _, _} = Imports.parse_import_line("fn foo(x: i64) i64 {\n")
    end
  end
end
