defmodule ZiglerTest.CompilerError.StructNotPubErrorTest do
  use ExUnit.Case, async: true

  describe "sema causes an error" do
    test "when a param-passed struct is not pub" do
      assert_raise CompileError,
                   "test/compiler_error/struct_not_pub_param.exs:10: the function `should_fail_param` accepts the struct `not_pub_struct` which is not public (defined at test/compiler_error/struct_not_pub_param.exs:6)",
                   fn ->
                     Code.compile_file("struct_not_pub_param.exs", __DIR__)
                   end
    end

    test "when a returned struct is not pub" do
      assert_raise CompileError,
                   "test/compiler_error/struct_not_pub_return.exs:10: the function `should_fail_return` returns the struct `not_pub_struct` which is not public (defined at test/compiler_error/struct_not_pub_return.exs:6)",
                   fn ->
                     Code.compile_file("struct_not_pub_return.exs", __DIR__)
                   end
    end
  end
end
