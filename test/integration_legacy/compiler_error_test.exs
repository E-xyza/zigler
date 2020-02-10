defmodule ZiglerTest.CompilerErrorTest do

  use ExUnit.Case, async: true

  test "compiler error catches and rewrites internal error" do
    test_file = "test/integration/assets/zig_syntax_error_module.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file, line: 15} = ce
    assert file =~ test_file
  end

  test "compiler error catches and rewrites external error" do
    test_file = "test/integration/assets/zig_syntax_bad_include.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file} = ce
    assert file =~ "test/integration/assets/bad.zig"
  end

  test "compiler error catches mismatched nif" do
    test_file = "test/integration/assets/mismatched_nif.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file, line: 5} = ce
    assert file =~ test_file
    assert ce.description =~ "nif docstring expecting \"oops\" not adjacent to function (next to \"two\")"
  end

  test "compiler error catches mismatched arity" do
    test_file = "test/integration/assets/mismatched_arity.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file, line: 6} = ce
    assert file =~ test_file
    assert ce.description =~ "mismatched arity"
  end

  test "compiler error catches missing nif" do
    test_file = "test/integration/assets/missing_nif.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file, line: 5} = ce
    assert ce.description =~ "missing function header for nif missing_nif"
  end

  test "compiler error catches bad parameter type" do
    test_file = "test/integration/assets/bad_parameter_type.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file, line: 5} = ce
    assert ce.description =~ "nif \"bad_param\" has unsupported parameter type u64"
  end

  test "compiler error catches bad retval type" do
    test_file = "test/integration/assets/bad_retval_type.exs"

    ce = assert_raise CompileError, fn ->
      Code.compile_file(test_file)
    end

    assert %CompileError{file: file, line: 5} = ce
    assert ce.description =~ "nif \"bad_retval\" has unsupported retval type u64"
  end

end
