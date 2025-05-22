defmodule ZiglerTest.CompilerError.TransitiveErrorTest do
  use ExUnit.Case, async: true

  describe "when a transitive error exists" do
    test "it raises a compiler error with the correct filename" do
      try do
        Code.compile_file("_transitive_error.exs", __DIR__)
      rescue
        e in CompileError ->
          assert Exception.message(e) =~
                   "_transitive_error.zig:2:25: error: expected 2 argument(s), found 1"
      else
        _ -> raise "Expected a compile error but none was raised"
      end
    end
  end
end
