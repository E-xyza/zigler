defmodule ZiglerTest.AllocatorTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn basic_allocate() usize {
      const ptr = beam.allocator.alloc(u8, 10_000) catch unreachable;
      return @intFromPtr(ptr.ptr);
  }

  pub fn basic_free(addr: usize) void {
      beam.allocator.free(@as([*]u8, @ptrFromInt(addr))[0..10_000]);
  }
  """

  describe "for the basic beam allocator" do
    test "allocate works" do
      basic_allocate()
    end

    test "free works" do
      addr = basic_allocate()
      basic_free(addr)
    end
  end

  ~Z"""
  pub fn large_allocate() usize {
      const ptr = beam.large_allocator.alloc(u8, 10_000) catch unreachable;
      return @intFromPtr(ptr.ptr);
  }

  pub fn large_allocate_aligned() usize {
      const ptr = beam.large_allocator.allocWithOptions(u8, 10_000, 1024, null) catch unreachable;
      return @intFromPtr(ptr.ptr);
  }

  pub fn large_free(addr: usize) void {
      beam.large_allocator.free(@as([*]u8, @ptrFromInt(addr))[0..10_000]);
  }
  """

  describe "for the large beam allocator" do
    test "allocate works" do
      large_allocate()
    end

    test "free works" do
      addr = large_allocate()
      large_free(addr)
    end

    test "allocate with alignment works" do
      addr = large_allocate_aligned()
      assert rem(addr, 1024) == 0
    end

    test "free with alignment works" do
      addr = large_allocate_aligned()
      large_free(addr)
    end
  end

  ~Z"""
  pub fn gpa_allocate() usize {
      const ptr = beam.general_purpose_allocator.alloc(u8, 10_000) catch unreachable;
      return @intFromPtr(ptr.ptr);
  }

  pub fn gpa_free(addr: usize) void {
      beam.general_purpose_allocator.free(@as([*]u8, @ptrFromInt(addr))[0..10_000]);
  }
  """

  describe "for the general purpose allocator" do
    test "allocate works" do
      gpa_allocate()
    end

    test "free works" do
      addr = gpa_allocate()
      gpa_free(addr)
    end
  end
end
