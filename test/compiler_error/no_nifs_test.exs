defmodule ZiglerTest.CompilerError.NoNifsTest do
  use ExUnit.Case, async: true

  describe "when a use Zig doesn't have any nifs" do
    test "it raises a compiler error" do
      assert_raise CompileError,
                   "test/compiler_error/_no_nifs.exs: no nifs found in module.",
                   fn ->
                     Code.compile_file("_no_nifs.exs", __DIR__)
                   end
    end
  end
end
