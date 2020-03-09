defmodule ZiglerTest.DryRun.SyntaxErrorTest do
  use ExUnit.Case, async: true

  def load(name) do
    __ENV__.file
    |> Path.dirname
    |> Path.join("assets/#{name}")
    |> Code.eval_file
  end

  @moduletag :syntax_error

  describe "a SyntaxError is thrown when a nif declaration" do
    test "doesn't match the function name" do
      file = "nif_name_mismatch.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "doesn't match function name"
      assert error.line == 5
      assert Path.basename(error.file) == file
    end

    test "doesn't match the function arity" do
      file = "nif_arity_mismatch.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "doesn't match the expected function arity"
      assert error.line == 5
      assert Path.basename(error.file) == file
    end
  end
end
