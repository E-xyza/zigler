defmodule ZiglerTest.Resource.AsBinaryTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, resources: [:StringResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  const Resource = beam.Resource;

  pub const StringResource = Resource([]u8, @import("root"), .{.Callbacks = StringResourceCallbacks});

  pub const StringResourceCallbacks = struct {
      pub fn dtor(_: beam.env, pid: *[]u8) void {
          beam.allocator.free(pid.*);
      }
  };

  pub fn create(src_string: []u8) StringResource {
      const new_string = beam.allocator.alloc(u8, src_string.len) catch unreachable;
      std.mem.copy(u8, new_string, src_string);
      return StringResource.create(new_string, .{}) catch unreachable;
  }

  pub const OutputModes = enum {binary, reference, static};

  pub fn create_manual(env: beam.env, src_string: []u8, mode: OutputModes) beam.term {
      const new_string = beam.allocator.alloc(u8, src_string.len) catch unreachable;
      std.mem.copy(u8, new_string, src_string);
      const resource = StringResource.create(new_string, .{}) catch unreachable;
      return switch (mode) {
        .binary => beam.make(env, resource, .{.output_as = .binary}),
        .reference => beam.make(env, resource, .{.output_as = .default}),
        .static => beam.make(env, resource, .{.output_as = .binary, .encoder = static_encoder}),
      };
  }

  fn static_encoder(_ : *const []u8) [] const u8 {
    return "barbaz";
  }

  pub fn unpack(resource: StringResource) []const u8 {
    return resource.unpack();
  }
  """

  describe "you can manually use beam.make to" do
    test "output as reference (default)" do
      ref_resource = create_manual("foobar", :reference)
      assert is_reference(ref_resource)
      assert ~C'foobar' == unpack(ref_resource)
    end

    test "output as binary" do
      ref_resource = create_manual("foobar", :binary)
      assert "foobar" == ref_resource
    end

    test "output with static encoder" do
      ref_resource = create_manual("foobar", :static)
      assert "barbaz" == ref_resource
    end
  end
end
