defmodule ZiglerTest.DryRun.SyntaxErrorTest do
  use ExUnit.Case, async: true

  def load(name) do
    __ENV__.file
    |> Path.dirname
    |> Path.join("assets/syntax_error/#{name}")
    |> Code.eval_file
  end

  @moduletag :syntax_error

  describe "a SyntaxError is thrown with the correct line" do
    test "when the sigil Z has no nifs" do
      file = "empty_sigil_z.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "doesn't contain any nifs"
      assert error.line == 4
      assert Path.basename(error.file) == file
    end

    test "when the sigil Z has no nifs on a second sigil Z" do
      file = "empty_second_sigil_z.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "doesn't contain any nifs"
      assert error.line == 11
      assert Path.basename(error.file) == file
    end

    test "when the sigil Z has a nif invalid return in the second sigil Z" do
      file = "nif_invalid_return_second_sigil_z.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.line == 16
      assert Path.basename(error.file) == file
    end
  end

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

    test "contains an invalid argument" do
      file = "nif_invalid_argument.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "demands an invalid parameter type"
      assert error.line == 6
      assert Path.basename(error.file) == file
    end

    test "contains an invalid return value" do
      file = "nif_invalid_return.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "returns an invalid type"
      assert error.line == 6
      assert Path.basename(error.file) == file
    end
  end

  describe "a SyntaxError is thrown when a resource" do
    test "declaration doesn't match the type const identifier" do
      file = "resource_name_mismatch.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "doesn't match succeeding const identifier"
      assert error.line == 8
      assert Path.basename(error.file) == file
    end

    test "cleanup doesn't have the correct arity" do
      file = "resource_cleanup_arity_mismatch.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "must have 2 arguments"
      assert error.line == 12
      assert Path.basename(error.file) == file
    end

    test "cleanup doesn't have the correct first argument" do
      file = "resource_cleanup_argument1_mismatch.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "must have first argument be of type"
      assert error.line == 12
      assert Path.basename(error.file) == file
    end

    test "cleanup doesn't have the correct second argument" do
      file = "resource_cleanup_argument2_mismatch.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "must have second argument be of type"
      assert error.line == 12
      assert Path.basename(error.file) == file
    end

    test "cleanup doesn't return correctly" do
      file = "resource_cleanup_return_mismatch.exs"
      error = assert_raise SyntaxError, fn -> load(file) end
      assert error.description =~ "must return `void`"
      assert error.line == 12
      assert Path.basename(error.file) == file
    end
  end
end
