defmodule ZiglerTest.Resource.UnusableTypeTest do
  # verifies that resources can carry "unusable types"


  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, resources: [:AnyOpaqueResource, :StructResource]

  ~Z"""
  const beam = @import("beam");
  pub const AnyOpaquePayload = *anyopaque;
  pub const StructPayload = struct { p: *anyopaque };

  pub const AnyOpaqueResource = beam.Resource(AnyOpaquePayload, @import("root"), .{});
  pub const StructResource = beam.Resource(StructPayload, @import("root"), .{});

  var value: u64 = undefined;

  pub fn make_ptr_resource() !AnyOpaqueResource {
      value = 47;
      return AnyOpaqueResource.create(&value, .{});
  }

  pub fn pull_ptr_resource(res: AnyOpaqueResource) u64 {
      const payload: *u64 = @ptrCast(@alignCast(res.unpack()));
      return payload.*;
  }

  pub fn make_struct_resource() !StructResource {
      value = 47;
      return StructResource.create(.{ .p = &value }, .{});
  }

  pub fn pull_struct_resource(res: StructResource) u64 {
      const payload: *u64 = @ptrCast(@alignCast(res.unpack().p));
      return payload.*;
  }
  """

  test "a resource wrapping an unusable anyopaque type" do
    assert 47 = make_ptr_resource() |> pull_ptr_resource()
  end

  test "a resource wrapping a struct with an unusable field type" do
    assert 47 = make_struct_resource() |> pull_struct_resource()
  end
end
