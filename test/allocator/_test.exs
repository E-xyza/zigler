defmodule ZiglerTest.AllocatorTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn raw_allocate() usize {
      const ptr = beam.raw_allocator.alloc(u8, 10_000) catch unreachable;
      return @intFromPtr(ptr.ptr);
  }

  pub fn raw_free(addr: usize) void {
      beam.raw_allocator.free(@as([*]u8, @ptrFromInt(addr))[0..10_000]);
  }
  """

  describe "for the raw beam allocator" do
    test "allocate works" do
      raw_allocate()
    end

    test "free works" do
      addr = raw_allocate()
      raw_free(addr)
    end
  end

  ~Z"""
  pub fn allocate() usize {
      const ptr = beam.allocator.alloc(u8, 10_000) catch unreachable;
      return @intFromPtr(ptr.ptr);
  }

  const std = @import("std");

  pub fn allocate_aligned() usize {
      const alignment = comptime std.mem.Alignment.fromByteUnits(1024);
      const ptr = beam.allocator.alignedAlloc(u8, alignment, 10_000) catch unreachable;
      return @intFromPtr(ptr.ptr);
  }

  pub fn free(addr: usize) void {
      beam.allocator.free(@as([*]u8, @ptrFromInt(addr))[0..10_000]);
  }
  """

  describe "for the large beam allocator" do
    test "allocate works" do
      allocate()
    end

    test "free works" do
      addr = allocate()
      free(addr)
    end

    test "allocate with alignment works" do
      addr = allocate_aligned()
      assert rem(addr, 1024) == 0
    end

    test "free with alignment works" do
      addr = allocate_aligned()
      free(addr)
    end
  end

  describe "for the debug allocator" do
    ~Z"""
    pub fn debug_allocate() usize {
        const ptr = beam.debug_allocator.alloc(u8, 10_000) catch unreachable;
        return @intFromPtr(ptr.ptr);
    }

    pub fn debug_free(addr: usize) void {
        beam.debug_allocator.free(@as([*]u8, @ptrFromInt(addr))[0..10_000]);
    }
    """

    test "allocate works" do
      debug_allocate()
    end

    test "free works" do
      addr = debug_allocate()
      debug_free(addr)
    end
  end
end
