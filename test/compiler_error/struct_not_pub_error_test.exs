defmodule ZiglerTest.CompilerError.StructNotPubErrorTest do
  use ExUnit.Case, async: true

  describe "sema causes an error" do
    test "when a param-passed struct is not pub" do
      assert_raise CompileError,
                   "test/compiler_error/struct_not_pub_param.exs:9: struct `not_pub_struct` (argument 1 of function `should_fail_param`) is not a `pub` struct",
                   fn ->
                     Code.compile_file("struct_not_pub_param.exs", __DIR__)
                   end
    end

    test "when a returned struct is not pub" do
      assert_raise CompileError,
                   "test/compiler_error/struct_not_pub_return.exs:9: struct `not_pub_struct` (return of function `should_fail_return`) is not a `pub` struct",
                   fn ->
                     Code.compile_file("struct_not_pub_return.exs", __DIR__)
                   end
    end
  end
end
