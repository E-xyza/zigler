defmodule ZiglerTest.CompilerError.RawFunctionThreadedTest do
  use ExUnit.Case, async: true

  describe "when you try to make a raw function threaded" do
    test "it raises a compiler error" do
      assert_raise CompileError,
                   "test/compiler_error/_raw_function_threaded.exs:1: the raw function `raw` may only be used with `:synchronous`, `:dirty_cpu` or `:dirty_io` concurrency",
                   fn ->
                     Code.compile_file("_raw_function_threaded.exs", __DIR__)
                   end
    end
  end
end