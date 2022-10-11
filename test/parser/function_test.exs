defmodule ZiglerTest.Parser.FunctionTest do
  use ExUnit.Case, async: true

  alias Zig.Parser

  describe "when given a private function" do
    test "it can be found" do
      assert %{functions: [%{name: :foo, code: []}]} = Parser.parse("fn foo() void {}")
    end
  end
end
