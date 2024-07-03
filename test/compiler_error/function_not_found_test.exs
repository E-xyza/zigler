defmodule ZiglerTest.CompilerError.FunctionNotFoundTest do
  use ExUnit.Case, async: true

  describe "when a use Zig doesn't have a specified function" do
    test "it raises a compiler error" do
      assert_raise CompileError,
                   "test/compiler_error/_function_not_found.exs:2: public function named `not_found_function` not found in semantic analysis of module.",
                   fn ->
                     Code.compile_file("_function_not_found.exs", __DIR__)
                   end
    end
  end
end
