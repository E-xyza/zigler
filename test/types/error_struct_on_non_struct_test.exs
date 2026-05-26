defmodule ZiglerTest.Types.ErrorStructOnNonStructTest do
  use ExUnit.Case, async: true
  require ZiglerTest.Compiler

  test "struct option requires struct return type" do
    assert_raise CompileError,
                 ~r"the `struct:` return option was specified for `returns_int` but the return type is `i32`, not a struct",
                 fn ->
                   ZiglerTest.Compiler.compile("_error_struct_on_non_struct.ex")
                 end
  end
end
