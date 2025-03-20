defmodule ZiglerTest.Allocator.CustomFunctionTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      basic: [allocator: :messaging_allocator],
      threaded: [:threaded, alias: :basic, allocator: :messaging_allocator]
    ]

  ~Z"""
  const std = @import("std");
  const beam = @import("beam");
  const Allocator = std.mem.Allocator;

  pub fn messaging_allocator() Allocator {
      return Allocator{
          .ptr = undefined,
          .vtable = &messaging_allocator_vtable,
      };
  }

  const messaging_allocator_vtable = Allocator.VTable{
      .alloc = messaging_alloc,
      .resize = messaging_resize,
      .remap = messaging_remap,
      .free = messaging_free,
  };

  fn messaging_alloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, retaddr: usize) ?[*]u8 {
      const pid = beam.self(.{}) catch unreachable;
      beam.send(pid, .{ .alloc, len }, .{}) catch unreachable;
      return beam.allocator.vtable.alloc(beam.allocator.ptr, len, alignment, retaddr);
  }

  fn messaging_resize(_: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, retaddr: usize) bool {
      return beam.allocator.vtable.resize(beam.allocator.ptr, buf, alignment, new_len, retaddr);
  }

  fn messaging_remap(context: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) ?[*]u8 {
      return beam.allocator.vtable.remap(context, memory, alignment, new_len, return_address);
  }

  fn messaging_free(_: *anyopaque, buf: []u8, alignment: std.mem.Alignment, retaddr: usize) void {
      const pid = beam.self(.{}) catch unreachable;
      beam.send(pid, .{ .free, buf.len }, .{}) catch unreachable;
      beam.allocator.vtable.free(beam.allocator.ptr, buf, alignment, retaddr);
  }

  pub fn basic(_: []u8) !void {
      _ = try beam.context.allocator.alloc(u8, 100);
  }
  """

  test "for synchronous function works" do
    basic("0123456789")
    assert_receive {:alloc, 10}
    assert_receive {:alloc, 100}
    assert_receive {:free, 10}
  end

  # this currently segfaults.  Fix in 0.14.0
  @tag :skip
  test "for threaded function works" do
    threaded("0123456789")
    assert_receive {:alloc, 10}
    assert_receive {:alloc, 100}
    assert_receive {:free, 10}
  end
end
