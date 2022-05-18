defmodule ZiglerTest.Integration.EnvTest do
  # tests to make sure that we can include beam.env terms in the nif
  # definition and have them compile correctly.

  use ExUnit.Case, async: true
  use Zig

  ~Z"""
  /// nif: zeroarity_with_env/0
  fn zeroarity_with_env(_: beam.env) i64 {
    return 47;
  }

  /// nif: zeroarity_with_erlnifenv/0
  fn zeroarity_with_erlnifenv(_: ?*e.ErlNifEnv) i64 {
    return 47;
  }
  """

  test "for a zero arity function env variables are valid first arguments" do
    assert 47 == zeroarity_with_env()
  end

  test "for a zero arity function erlnifenv variables are valid first arguments" do
    assert 47 == zeroarity_with_erlnifenv()
  end

  ~Z"""
  /// nif: int_with_env/1
  fn int_with_env(_: beam.env, val: i64) i64 {
    return val;
  }

  /// nif: int_with_erlnifenv/1
  fn int_with_erlnifenv(_: ?*e.ErlNifEnv, val: i64) i64 {
    return val;
  }
  """

  test "env variables are valid first arguments" do
    assert 47 == int_with_env(47)
  end

  test "erlnifenv variables are valid first arguments" do
    assert 47 == int_with_erlnifenv(47)
  end
end
