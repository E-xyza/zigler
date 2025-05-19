defmodule ZiglerTest.Cxx.CompileErrorTest do
  use ExUnit.Case, async: true

  describe "when a use Zig doesn't have a specified function" do
    test "it raises a compiler error" do
      file = Path.expand("src2/compile_error.c", __DIR__)
      expected_message = "#{file}:2:3: error: use of undeclared identifier 'unt'"

      try do
        Code.compile_file("_compile_error.exs", __DIR__)
      rescue
        e in CompileError ->
          assert Exception.message(e) =~ expected_message
      else
        _ ->
          raise "Expected a compile error but none was raised"
      end
    end
  end
end
