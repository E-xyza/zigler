defmodule ZiglerTest.Integration.Strategies.AllocatorsTest do
  use ExUnit.Case, async: true

  use Zig

  ~Z"""
  const testing = std.testing;
  const mem = std.mem;

  /// nif: test_beam_allocator/0
  fn test_beam_allocator() void {
    std.heap.testAllocator(beam.allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.allocator) catch unreachable;
    //std.heap.testAllocatorAligned(beam.allocator) catch unreachable;
    //std.heap.testAllocatorAlignedShrink(beam.allocator) catch unreachable;
  }

  /// nif: test_beam_large_allocator/0
  fn test_beam_large_allocator() void {
    std.heap.testAllocator(beam.allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.large_allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.large_allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.large_allocator) catch unreachable;
    //std.heap.testAllocatorAligned(beam.allocator) catch unreachable;
    std.heap.testAllocatorLargeAlignment(beam.large_allocator) catch unreachable;
    std.heap.testAllocatorAlignedShrink(beam.large_allocator) catch unreachable;
  }

  /// nif: test_beam_general_purpose_allocator/0
  fn test_beam_general_purpose_allocator() void {
    std.heap.testAllocator(beam.allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.general_purpose_allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.general_purpose_allocator) catch unreachable;
    std.heap.testAllocatorAligned(beam.general_purpose_allocator) catch unreachable;
    //std.heap.testAllocatorAligned(beam.allocator) catch unreachable;
    std.heap.testAllocatorLargeAlignment(beam.general_purpose_allocator) catch unreachable;
    std.heap.testAllocatorAlignedShrink(beam.general_purpose_allocator) catch unreachable;
  }
  """

  @tag :skip
  test "allocator stress test" do
    test_beam_allocator()
    test_beam_large_allocator()
    test_beam_general_purpose_allocator()
  end

  # proves that you can keep memory around in global memory state between
  # calls.  In real code, you would want to protect this memory with a resource
  # so that it can be tied to erlang GC.

  ~Z"""
  var global_slice : []u8 = undefined;

  /// nif: allocate/1
  fn allocate(_: ?*e.ErlNifEnv, length: i64) bool {

    var usize_length = @intCast(usize, length);

    global_slice = beam.allocator.alloc(u8, usize_length) catch {
      // don't do this in real life!
      unreachable;
    };
    // NB: don't defer a free here (don't do this in real life!!!)

    // fill the slice with letters
    for (global_slice) | _, i | {
      global_slice[i] = 97 + @intCast(u8, i);
    }

    return true;
  }

  /// nif: fetch/0
  fn fetch(env: ?*e.ErlNifEnv) e.ErlNifTerm {
    return e.enif_make_atom_len(env, global_slice.ptr, global_slice.len);
  }
  """

  test "elixir persistent memory works" do
    assert true == allocate(2)
    assert :ab == fetch()
  end

end
