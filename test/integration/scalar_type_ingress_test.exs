defmodule ZiglerTest.Integration.ScalarTypeIngressTest do
  use ExUnit.Case, async: true
  use Zig

  ~Z"""
  /// nif: ingress_bool/1
  fn ingress_bool(val: bool) bool {
    return val;
  }
  """

  test "ingressing bool values works" do
    assert ingress_bool(true)
    refute ingress_bool(false)

    assert_raise FunctionClauseError, fn ->
      ingress_bool("true")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_bool("false")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_bool(1)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_bool(0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_bool(nil)
    end
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

    # type testing
    assert_raise FunctionClauseError, fn ->
      ingress_i32("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_i64("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_i32(47.0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_i64(47.0)
    end

    # bounds testing
    assert_raise FunctionClauseError, fn ->
      ingress_i32(0x8000_0000)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_i32(-0x8000_0001)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_i64(0x8000_0000_0000_0000)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_i64(-0x8000_0000_0000_0001)
    end
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

    # type testing
    assert_raise FunctionClauseError, fn ->
      ingress_u8("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u16("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u32("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u64("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u8(47.0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u16(47.0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u32(47.0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u64(47.0)
    end

    # bounds testing
    assert_raise FunctionClauseError, fn ->
      ingress_u8(-1)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u16(-1)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u32(-1)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u64(-1)
    end

    # bounds testing
    assert_raise FunctionClauseError, fn ->
      ingress_u8(0x100)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u16(0x1_0000)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u32(0x1_0000_0000)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_u64(0x1_0000_0000_0000_0000)
    end
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
  fn ingress_c_long(val: c_long) c_long {
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

    # type testing
    assert_raise FunctionClauseError, fn ->
      ingress_c_int("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_c_uint("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_c_long("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_c_ulong("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_c_int(47.0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_c_uint(47.0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_c_long(47.0)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_c_ulong(47.0)
    end
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

    # type errors
    assert_raise FunctionClauseError, fn ->
      ingress_f16("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_f32("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_f64("47")
    end

    assert_raise FunctionClauseError, fn ->
      ingress_f16(47)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_f32(47)
    end

    assert_raise FunctionClauseError, fn ->
      ingress_f64(47)
    end
  end
end
