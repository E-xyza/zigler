defmodule ZiglerTest.Integration.ScalarTypeEgressTest do
  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  /// nif: egress_true/0
  fn egress_true() bool {
    return true;
  }

  /// nif: egress_false/0
  fn egress_false() bool {
    return false;
  }
  """

  test "egressing bool values works" do
    assert egress_true()
    refute egress_false()
  end

  ~Z"""
  /// nif: egress_i32/0
  fn egress_i32() i32 {
    return 47;
  }

  /// nif: egress_i64/0
  fn egress_i64() i64 {
    return 47;
  }
  """

  test "egressing signed integer values works" do
    assert 47 === egress_i32()
    assert 47 === egress_i64()
  end

  ~Z"""
  /// nif: egress_u8/0
  fn egress_u8() u8 {
    return 47;
  }

  /// nif: egress_u16/0
  fn egress_u16() u16 {
    return 47;
  }

  /// nif: egress_u32/0
  fn egress_u32() u32 {
    return 47;
  }

  /// nif: egress_u64/0
  fn egress_u64() u64 {
    return 47;
  }
  """

  test "egressing unsigned integer values works" do
    assert 47 === egress_u8()
    assert 47 === egress_u16()
    assert 47 === egress_u32()
    assert 47 === egress_u64()
  end

  ~Z"""
  /// nif: egress_c_int/0
  fn egress_c_int() c_int {
    return 47;
  }

  /// nif: egress_c_uint/0
  fn egress_c_uint() c_uint {
    return 47;
  }

  /// nif: egress_c_long/0
  fn egress_c_long() c_long {
    return 47;
  }

  /// nif: egress_c_ulong/0
  fn egress_c_ulong() c_ulong {
    return 47;
  }
  """

  test "egressing c integer types works" do
    assert 47 === egress_c_int()
    assert 47 === egress_c_uint()
    assert 47 === egress_c_long()
    assert 47 === egress_c_ulong()
  end

  ~Z"""
  /// nif: egress_f16/0
  fn egress_f16() f16 {
    return 47.0;
  }

  /// nif: egress_f32/0
  fn egress_f32() f32 {
    return 47.0;
  }

  /// nif: egress_f64/0
  fn egress_f64() f64 {
    return 47.0;
  }
  """

  test "egressing floating point values works" do
    assert 47.0 === egress_f16()
    assert 47.0 === egress_f32()
    assert 47.0 === egress_f64()
  end
end
