defmodule ZiglerTest.CompilerErrorTest do

  use ExUnit.Case, async: true

  test "compiler error catches and rewrites internal error" do
    test_file = "test/assets/zig_syntax_error_module.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file, line: 14} = ce
    assert file =~ test_file
  end

  test "compiler error catches and rewrites external error" do
    test_file = "test/assets/zig_syntax_bad_include.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file} = ce
    assert file =~ "test/assets/bad.zig"
  end

  test "compiler error catches mismatched nif" do
    test_file = "test/assets/mismatched_nif.exs"

    assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end
  end

  test "compiler error catches mismatched arity" do
    test_file = "test/assets/mismatched_arity.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert ce.description =~ "mismatch of arity"
  end

end
