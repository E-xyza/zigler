defmodule ZiglerTest.Integration.ScalarTypeIngressTest do
  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  /// nif: ingress_bool/1
  fn ingress_bool(val: bool) bool {
    return val;
  }
  """

  test "ingressing bool values works" do
    assert ingress_bool(true)
    refute ingress_bool(false)
  end

  ~Z"""
  /// nif: ingress_i32/1
  fn ingress_i32(val: i32) i32 {
    return val;
  }

  /// nif: ingress_i64/1
  fn ingress_i64(val: i64) i64 {
    return val;
  }
  """

  test "ingressing signed integer values works" do
    assert 47 === ingress_i32(47)
    assert 47 === ingress_i64(47)
  end

  ~Z"""
  /// nif: ingress_u8/1
  fn ingress_u8(val: u8) u8 {
    return val;
  }

  /// nif: ingress_u16/1
  fn ingress_u16(val: u16) u16 {
    return val;
  }

  /// nif: ingress_u32/1
  fn ingress_u32(val: u32) u32 {
    return val;
  }

  /// nif: ingress_u64/1
  fn ingress_u64(val: u64) u64 {
    return val;
  }
  """

  test "ingressing unsigned integer values works" do
    assert 47 === ingress_u8(47)
    assert 47 === ingress_u16(47)
    assert 47 === ingress_u32(47)
    assert 47 === ingress_u64(47)
  end

  ~Z"""
  /// nif: ingress_c_int/1
  fn ingress_c_int(val: c_int) c_int {
    return val;
  }

  /// nif: ingress_c_uint/1
  fn ingress_c_uint(val: c_uint) c_uint {
    return val;
  }

  /// nif: ingress_c_long/1
  fn ingress_c_long(val: c_ulong) c_long {
    return val;
  }

  /// nif: ingress_c_ulong/1
  fn ingress_c_ulong(val: c_ulong) c_ulong {
    return val;
  }
  """

  test "ingressing c integer types works" do
    assert 47 === ingress_c_int(47)
    assert 47 === ingress_c_uint(47)
    assert 47 === ingress_c_long(47)
    assert 47 === ingress_c_ulong(47)
  end

  ~Z"""
  /// nif: ingress_f16/1
  fn ingress_f16(val: f16) f16 {
    return val;
  }

  /// nif: ingress_f32/1
  fn ingress_f32(val: f32) f32 {
    return val;
  }

  /// nif: ingress_f64/1
  fn ingress_f64(val: f64) f64 {
    return val;
  }
  """

  test "ingressing floating point values works" do
    assert 47.0 === ingress_f16(47.0)
    assert 47.0 === ingress_f32(47.0)
    assert 47.0 === ingress_f64(47.0)
  end
end
