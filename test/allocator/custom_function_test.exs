defmodule ZiglerTest.Allocator.CustomFunctionTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [go: [allocator: :messaging_allocator]]

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
      .free = messaging_free,
  };

  fn messaging_alloc(_: *anyopaque, len: usize, log2_align: u8, retaddr: usize) ?[*]u8 {
      const pid = beam.self(.{}) catch unreachable;
      beam.send(pid, .{ .alloc, len }, .{}) catch unreachable;
      return beam.allocator.vtable.alloc(beam.allocator.ptr, len, log2_align, retaddr);
  }

  fn messaging_resize(_: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, retaddr: usize) bool {
      return beam.allocator.vtable.resize(beam.allocator.ptr, buf, log2_buf_align, new_len, retaddr);
  }

  fn messaging_free(_: *anyopaque, buf: []u8, log2_align: u8, retaddr: usize) void {
      const pid = beam.self(.{}) catch unreachable;
      beam.send(pid, .{ .free, buf.len }, .{}) catch unreachable;
      beam.allocator.vtable.free(beam.allocator.ptr, buf, log2_align, retaddr);
  }

  pub fn go(_: []u8) !void {
      _ = try beam.context.allocator.alloc(u8, 100);
  }
  """

  test "directly using file works" do
    go("0123456789")
    assert_receive {:alloc, 10}
    assert_receive {:alloc, 100}
    assert_receive {:free, 10}
  end
end
