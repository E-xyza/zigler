defmodule ZiglerTest.Typing do

  @moduledoc false

  use Zigler, app: :zigler

  ~Z"""
  /// nif: zero_arity/0
  fn zero_arity() i64 {
    return 1;
  }

  /// nif: void_return/0
  fn void_return() void {
  }

  /// nif: u8_identity/1
  fn u8_identity(a: u8) u8 {
    return a;
  }

  /// nif: c_int_identity/1
  fn c_int_identity(a: c_int) c_int {
    return a;
  }

  /// nif: c_long_identity/1
  fn c_long_identity(a: c_long) c_long {
    return a;
  }

  /// nif: isize_identity/1
  fn isize_identity(a: isize) isize {
    return a;
  }

  /// nif: usize_identity/1
  fn usize_identity(a: usize) usize {
    return a;
  }

  /// nif: i32_identity/1
  fn i32_identity(a: i32) i32 {
    return a;
  }

  /// nif: i64_identity/1
  fn i64_identity(a: i64) i64 {
    return a;
  }

  /// nif: f16_identity/1
  fn f16_identity(a: f16) f16 {
    return a;
  }

  /// nif: f32_identity/1
  fn f32_identity(a: f32) f32 {
    return a;
  }

  /// nif: f64_identity/1
  fn f64_identity(a: f64) f64 {
    return a;
  }

  /// nif: bool_identity/1
  fn bool_identity(a: bool) bool {
    return a;
  }

  /// nif: term_identity_a/1
  fn term_identity_a(a: beam.term) beam.term {
    return a;
  }

  /// nif: term_identity_b/1
  fn term_identity_b(a: e.ErlNifTerm) e.ErlNifTerm {
    return a;
  }

  /// nif: c_string_identity/1
  fn c_string_identity(a: [*c]u8) [*c]u8 {
    return a;
  }

  /// nif: slice_identity/1
  fn slice_identity(a: []u8) []u8 {
    return a;
  }

  /// nif: i32_slice_identity/1
  fn i32_slice_identity(a: []i32) []i32 {
    return a;
  }

  /// nif: i64_slice_identity/1
  fn i64_slice_identity(a: []i64) []i64 {
    return a;
  }

  /// nif: f16_slice_identity/1
  fn f16_slice_identity(a: []f16) []f16 {
    return a;
  }

  /// nif: f32_slice_identity/1
  fn f32_slice_identity(a: []f32) []f32 {
    return a;
  }

  /// nif: f64_slice_identity/1
  fn f64_slice_identity(a: []f64) []f64 {
    return a;
  }

  """
end
