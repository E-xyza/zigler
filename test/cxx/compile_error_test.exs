defmodule ZiglerTest.Cxx.CompileErrorTest do
  use ExUnit.Case, async: true

  describe "when a use Zig doesn't have a specified function" do
    test "it raises a compiler error" do
      try do
        Code.compile_file("_compile_error.exs", __DIR__)
      rescue
        e in CompileError ->
          assert Exception.message(e) =~
                   "compile_error.c:2:3: error: use of undeclared identifier 'unt'"
      else
        _ ->
          raise "Expected a compile error but none was raised"
      end
    end
  end
end
