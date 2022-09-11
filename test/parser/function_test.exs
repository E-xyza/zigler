defmodule ZiglerTest.Parser.FunctionTest do
  use ExUnit.Case, async: true

  alias Zig.Parser

  describe "when given a function" do
    test "it can be identified" do
      assert [
               %{
                 type: :fun,
                 name: "foo",
                 file: "file",
                 line: 1
               }
             ] =
               Parser.parse_decls(
                 """
                 fn foo() void {}
                 """,
                 file: "file"
               )

      assert [
               %{
                 type: :fun,
                 name: "foo",
                 file: "file",
                 line: 2
               }
             ] =
               Parser.parse_decls(
                 """

                 fn foo() void {}
                 """,
                 file: "file"
               )
    end
  end
end
