defmodule ZiglerTest.CXX.ErrorInOutOnNonPointerTest do
  use ExUnit.Case, async: true
  require ZiglerTest.Compiler

  test "in_out option requires pointer-like parameter type" do
    assert_raise CompileError,
                 ~r"nif function `takes_int` cannot have a an in-out parameter of type i32",
                 fn ->
                   ZiglerTest.Compiler.compile("error_in_out_on_non_pointer.ex")
                 end
  end
end
