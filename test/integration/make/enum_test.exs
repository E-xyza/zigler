defmodule ZiglerTest.Make.EnumTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn make_anonymous_enum() beam.term {
    return beam.make(.foo, .{});
  }
  """

  test "an anonymous enum is returned as an atom" do
    assert :foo = make_anonymous_enum()
  end

  ~Z"""
  const e = enum { bar };

  pub fn make_enum() beam.term {
    return beam.make(e.bar, .{});
  }
  """

  test "a typed enum is also returned as an atom" do
    assert :bar = make_enum()
  end

  ~Z"""
  const err = error { baz };

  pub fn make_error() beam.term {
    return beam.make(err.baz, .{});
  }
  """

  test "an error is also returned as an atom" do
    assert :baz = make_error()
  end
end
