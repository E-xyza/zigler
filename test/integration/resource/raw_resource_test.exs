defmodule ZiglerTest.Resource.BasicResourceTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, resources: [:Struct, :u64]

  ~Z"""
  const beam = @import("beam");
  const Resource = beam.Resource;
  pub const resource = beam.resources(@import("root"));

  pub const Struct = struct {
      payload: u64,
  };

  pub fn new_scalar(res: u64) Resource(u64) {
      return resource.create(res, .{}) catch unreachable;
  }

  pub fn unpack_scalar(res: Resource(u64)) u64 {
    return resource.unpack(res);
  }

  pub fn increment_scalar(res: Resource(u64)) void {
    const to_increment = resource.unpack(res);
    return resource.update(res, to_increment + 1);
  }

  pub fn new_struct(res: Struct) Resource(Struct) {
      return resource.create(res, .{}) catch unreachable;
  }

  pub fn unpack_struct(res: Resource(Struct)) u64 {
    return resource.unpack(res).payload;
  }

  pub fn increment_struct(res: Resource(Struct)) void {
    const to_increment_struct = resource.unpack(res);
    return resource.update(res, .{.payload = to_increment_struct.payload + 1});
  }
  """

  test "basic scalar resources work" do
    res = new_scalar(47)
    assert is_reference(res)
    assert 47 = unpack_scalar(res)
    increment_scalar(res)
    assert 48 = unpack_scalar(res)
  end

  test "basic struct resources work" do
    res = new_struct(%{payload: 47})
    assert is_reference(res)
    assert 47 = unpack_struct(res)
    increment_struct(res)
    assert 48 = unpack_struct(res)
  end
end
