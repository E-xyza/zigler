defmodule ZiglerTest.CompilerError.SyntaxErrorTest do
  use ExUnit.Case, async: true

  describe "sema causes an error" do
    case :os.type() do
      {_, :nt} ->
        @syntax_error "test\\compiler_error\\.Elixir.ZiglerTest.CompilerError.SyntaxError.zig:5:12: error: expected ';' after statement\r\n  return 42 // note a semicolon is missing here.\r\n           ^"

      _ ->
        @syntax_error String.trim("""
                      test/compiler_error/_syntax_error.exs:8: error: expected ';' after statement

                        return 42 // note a semicolon is missing here.
                                 ^
                      """)
    end

    test "when the zig syntax is wrong" do
      assert_raise CompileError,
                   @syntax_error,
                   fn ->
                     Code.compile_file("_syntax_error.exs", __DIR__)
                   end
    end
  end
end
