defmodule ZiglerTest.Integration.StringTest do
  use ExUnit.Case, async: true
  use Zig

  ~Z"""
  /// nif: ingress_string/1
  fn ingress_string(string: []u8) i64 {
    _ = string;
    return 47;
  }
  """

  test "ingressing strings" do
    assert 47 == ingress_string("hello")
    assert_raise FunctionClauseError, fn -> ingress_string('hello') end
    assert_raise FunctionClauseError, fn -> ingress_string(:hello) end
  end

  ~Z"""
  /// nif: egress_string/1
  fn egress_string(string: []u8) []u8 {
    return string;
  }
  """

  test "egressing strings" do
    assert "hello" == egress_string("hello")
    assert_raise FunctionClauseError, fn -> ingress_string('hello') end
    assert_raise FunctionClauseError, fn -> ingress_string(:hello) end
  end

end
