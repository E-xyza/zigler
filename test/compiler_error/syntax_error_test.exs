defmodule ZiglerTest.CompilerError.SyntaxErrorTest do
  use ExUnit.Case, async: true

  describe "sema causes an error" do
    @syntax_error ~r|test/compiler_error/_syntax_error.exs:8: error: expected ';' after statement|

    test "when the zig syntax is wrong" do
      assert_raise CompileError,
                   @syntax_error,
                   fn ->
                     Code.compile_file("_syntax_error.exs", __DIR__)
                   end
    end
  end
end
