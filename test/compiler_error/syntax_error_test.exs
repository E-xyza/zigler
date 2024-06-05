defmodule ZiglerTest.CompilerError.SyntaxErrorTest do
  use ExUnit.Case, async: true

  describe "sema causes an error" do
    test "when the zig syntax is wrong" do
      assert_raise CompileError,
                   "test/compiler_error/syntax_error.exs:8: error: expected ';' after statement",
                   fn ->
                     Code.compile_file("syntax_error.exs", __DIR__)
                   end
    end
  end
end
