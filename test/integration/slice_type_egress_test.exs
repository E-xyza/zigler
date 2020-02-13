defmodule ZiglerTest.Integration.SliceTypeEgressTest do
  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  var empty_i32 = [_]i32 { };
  /// nif: egress_i32_slice/0
  fn egress_i32_slice() []i32 {
    var data = beam.allocator.alloc(i32, 4) catch return empty_i32[0..0];
    // don't bother freeing it (this is normally a memory leak)

    for (data) |*x| { x.* = 47; }

    return data;
  }

  var empty_i64 = [_]i64 { };
  /// nif: egress_i64_slice/0
  fn egress_i64_slice() []i64 {
    var data = beam.allocator.alloc(i64, 4) catch return empty_i64[0..0];
    // don't bother freeing it (this is normally a memory leak)

    for (data) |*x| { x.* = 47; }

    return data;
  }
  """

  test "egressing integers" do
    assert [47, 47, 47, 47] == egress_i32_slice()
    assert [47, 47, 47, 47] == egress_i64_slice()
  end

end
