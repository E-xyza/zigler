defmodule ZiglerTest.Resource.BasicTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, resources: [:StructResource, :U64Resource]

  ~Z"""
  const beam = @import("beam");
  const Resource = beam.Resource;
  const root = @import("root");

  pub const Struct = struct {
      payload: u64,
  };

  pub const StructResource = Resource(Struct, root, .{});
  pub const U64Resource = Resource(u64, root, .{});

  pub fn new_scalar(resource: u64) U64Resource {
      return U64Resource.create(resource) catch unreachable;
  }

  pub fn unpack_scalar(resource: U64Resource) u64 {
    return resource.unpack();
  }

  pub fn increment_scalar(resource: U64Resource) void {
    const to_increment = resource.unpack();
    return resource.update(to_increment + 1);
  }

  pub fn new_struct(s: Struct) StructResource {
      return StructResource.create(s) catch unreachable;
  }

  pub fn unpack_struct(resource: StructResource) Struct {
    return resource.unpack();
  }

  pub fn increment_struct(resource: StructResource) void {
    const to_increment_struct = resource.unpack();
    return resource.update(.{.payload = to_increment_struct.payload + 1});
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
    assert %{payload: 47} = unpack_struct(res)
    increment_struct(res)
    assert %{payload: 48} = unpack_struct(res)
  end
end
