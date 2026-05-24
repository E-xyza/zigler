defmodule ZiglerTest.CornerCases.RecursiveResourceDefinition do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler, resources: [:PointerResource]

  ~Z"""
  const beam = @import("beam");
  const root = @import("root");

  const LinkedList = struct { payload: u64, next: ?*LinkedList };

  pub const PointerResource = beam.Resource(*LinkedList, root, .{});

  pub fn create_rsrc(number: u64) !PointerResource {
      const new_struct = try beam.allocator.create(LinkedList);
      new_struct.* = .{ .payload = number, .next = null };
      return PointerResource.create(new_struct, .{});
  }
  """

  test "recursive resource pointer works" do
    assert is_reference(create_rsrc(47))
  end
end
