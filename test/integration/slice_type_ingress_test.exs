defmodule ZiglerTest.Integration.SliceTypeIngressTest do
  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  /// nif: ingress_i32_slice/1
  fn ingress_i32_slice(slice: []i32) []i32 {
    return slice;
  }

  /// nif: ingress_i64_slice/1
  fn ingress_i64_slice(slice: []i64) []i64 {
    return slice;
  }
  """

  test "egressing integers" do
    assert [47, 47, 47, 47] == ingress_i32_slice([47, 47, 47, 47])
    assert [47, 47, 47, 47] == ingress_i64_slice([47, 47, 47, 47])
  end

  ~Z"""
  /// nif: ingress_f32_slice/1
  fn ingress_f32_slice(slice: []f32) []f32 {
    return slice;
  }

  /// nif: ingress_f64_slice/1
  fn ingress_f64_slice(slice: []f64) []f64 {
    return slice;
  }
  """

  test "egressing floats" do
    assert [47.0, 47.0, 47.0, 47.0] == ingress_f32_slice([47.0, 47.0, 47.0, 47.0])
    assert [47.0, 47.0, 47.0, 47.0] == ingress_f64_slice([47.0, 47.0, 47.0, 47.0])
  end

end
