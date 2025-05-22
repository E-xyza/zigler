defmodule ZiglerTest.Resource.UnusableTypeTest do
  # verifies that resources can carry "unusable types"


  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, resources: [:Resource]

  ~Z"""
  const beam = @import("beam");
  pub const Payload = *anyopaque;

  pub const Resource = beam.Resource(Payload, @import("root"), .{});

  var value: u64 = undefined;

  pub fn make_resource() !Resource {
      value = 47;
      return Resource.create(&value, .{});
  }

  pub fn pull_resource(res: Resource) u64 {
      const payload: *u64 = @ptrCast(@alignCast(res.unpack()));
      return payload.*;
  }
  """

  test "a resource wrapping an unusable type" do
    assert 47 = make_resource() |> pull_resource()
  end
end
