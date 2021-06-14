defmodule ZiglerTest.Integration.ErrorTest do
  use ExUnit.Case, async: true
  use Zig

  ~Z"""
  /// nif: void_error/1
  fn void_error(input: i64) !void {
    if (input != 47) {
      return error.BadInput;
    }
  }

  /// nif: union_error/1
  fn union_error(input: i64) !i64 {
    if (input == 42) {
      return error.BadInput;
    }
    return input;
  }
  """

  test "for the void error case" do
    assert :ok == void_error(47)

    void_error(42)
  end

  test "for the error set union case" do
  end

end
