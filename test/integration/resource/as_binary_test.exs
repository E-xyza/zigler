defmodule ZiglerTest.Resource.AsBinaryTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    resources: [:StringResource],
    nifs: [{:output_as_binary, return: :binary}, ...]

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

  pub fn output_auto(src_string: []u8) StringResource {
      const new_string = beam.allocator.alloc(u8, src_string.len) catch unreachable;
      std.mem.copy(u8, new_string, src_string);
      return StringResource.create(new_string, .{}) catch unreachable;
  }

  pub const OutputModes = enum {binary, reference, static};

  pub fn output_manual(env: beam.env, src_string: []u8, mode: OutputModes) beam.term {
      const new_string = beam.allocator.alloc(u8, src_string.len) catch unreachable;
      std.mem.copy(u8, new_string, src_string);
      const resource = StringResource.create(new_string, .{}) catch unreachable;
      return switch (mode) {
        .binary => beam.make(env, resource, .{.output_as = .binary}),
        .reference => beam.make(env, resource, .{.output_as = .default}),
        .static => beam.make(env, resource, .{.output_as = .binary, .encoder = static_encoder}),
      };
  }

  pub fn output_as_binary(src_string: []u8) StringResource {
    const new_string = beam.allocator.alloc(u8, src_string.len) catch unreachable;
    std.mem.copy(u8, new_string, src_string);
    return StringResource.create(new_string, .{}) catch unreachable;
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
      ref_resource = output_manual("foobar", :reference)
      assert is_reference(ref_resource)
      assert ~C'foobar' == unpack(ref_resource)
    end

    test "output as binary" do
      str_resource = output_manual("foobar", :binary)
      assert "foobar" == str_resource
    end

    test "output with static encoder" do
      str_resource = output_manual("foobar", :static)
      assert "barbaz" == str_resource
    end
  end

  describe "function marshalling uses" do
    test "output as reference (default)" do
      ref_resource = output_auto("foobar")
      assert is_reference(ref_resource)
      assert ~C'foobar' == unpack(ref_resource)
    end

    test "output as binary, determined by return clause" do
      str_resource = output_as_binary("foobar")
      assert "foobar" == str_resource
    end
  end
end
