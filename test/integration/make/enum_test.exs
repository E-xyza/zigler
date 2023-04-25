defmodule ZiglerTest.Make.EnumTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn make_anonymous_enum(env: beam.env) beam.term {
    return beam.make(env, .foo, .{});
  }
  """

  test "an anonymous enum is returned as an atom" do
    assert :foo = make_anonymous_enum()
  end

  ~Z"""
  const e = enum { bar };

  pub fn make_enum(env: beam.env) beam.term {
    return beam.make(env, e.bar, .{});
  }
  """

  test "a typed enum is also returned as an atom" do
    assert :bar = make_enum()
  end

  ~Z"""
  const err = error { baz };

  pub fn make_error(env: beam.env) beam.term {
    return beam.make(env, err.baz, .{});
  }
  """

  test "an error is also returned as an atom" do
    assert :baz = make_error()
  end
end
