defmodule ZiglerTest.Parser.DocsTest do
  use ExUnit.Case, async: true

  alias Zig.Doc.Parser

  @moduletag :parser

  test "error structs are parsed correctly" do
    assert {:ok, [err: ["Foo", {:doc, ["an error enum\n"]}, "FooError"]], ";\n", _, _, _} =
             Parser.error_head("""
             pub const Foo = error {
               /// an error enum
               FooError
             };
             """)
  end
end
