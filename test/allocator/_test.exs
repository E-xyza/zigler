defmodule ZiglerTest.AllocatorTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  var raw_slice: []u8 = undefined;

  pub fn raw_allocate() usize {
      raw_slice = beam.raw_allocator.alloc(u8, 10_000) catch unreachable;
      return @intFromPtr(raw_slice.ptr);
  }

  pub fn raw_free() void {
      beam.raw_allocator.free(raw_slice);
  }
  """

  describe "for the raw beam allocator" do
    test "allocate works" do
      raw_allocate()
    end

    test "free works" do
      raw_allocate()
      raw_free()
    end
  end

  ~Z"""
  const std = @import("std");

  var large_slice: []u8 = undefined;
  var aligned_slice: []align(1024) u8 = undefined;

  pub fn allocate() usize {
      large_slice = beam.allocator.alloc(u8, 10_000) catch unreachable;
      return @intFromPtr(large_slice.ptr);
  }

  pub fn allocate_aligned() usize {
      const alignment = comptime std.mem.Alignment.fromByteUnits(1024);
      aligned_slice = beam.allocator.alignedAlloc(u8, alignment, 10_000) catch unreachable;
      return @intFromPtr(aligned_slice.ptr);
  }

  pub fn free() void {
      beam.allocator.free(large_slice);
  }

  pub fn free_aligned() void {
      beam.allocator.free(aligned_slice);
  }
  """

  describe "for the large beam allocator" do
    test "allocate works" do
      allocate()
    end

    test "free works" do
      allocate()
      free()
    end

    test "allocate with alignment works" do
      addr = allocate_aligned()
      assert rem(addr, 1024) == 0
      free_aligned()
    end

    test "free with alignment works" do
      allocate_aligned()
      free_aligned()
    end
  end

  describe "for the debug allocator" do
    ~Z"""
    var debug_slice: []u8 = undefined;

    pub fn debug_allocate() usize {
        debug_slice = beam.debug_allocator.alloc(u8, 10_000) catch unreachable;
        return @intFromPtr(debug_slice.ptr);
    }

    pub fn debug_free() void {
        beam.debug_allocator.free(debug_slice);
    }
    """

    test "allocate works" do
      debug_allocate()
    end

    test "free works" do
      debug_allocate()
      debug_free()
    end
  end
end
