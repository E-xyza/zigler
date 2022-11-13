
///////////////////////////////////////////////////////////////////////////////
// BEAM allocator definitions
///////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const e = @import("erl_nif.zig");

const Allocator = std.mem.Allocator;

// basic allocator

/// !value
/// provides a default BEAM allocator.  This is an implementation of the Zig
/// allocator interface.  Use `beam.allocator.alloc` everywhere to safely
/// allocate slices efficiently, and use `beam.allocator.free` to release that
/// memory.  For single item allocation, use `beam.allocator.create` and
/// `beam.allocator.destroy` to release the memory.
///
/// Note this does not make the allocated memory *garbage collected* by the
/// BEAM.
///
/// All memory will be tracked by the beam.  All allocations happen with 8-byte
/// alignment, as described in `erl_nif.h`.  This is sufficient to create
/// correctly aligned `beam.terms`, and for most purposes.
/// For data that require greater alignment, use `beam.large_allocator`.
///
/// ### Example
///
/// The following code will return ten bytes of new memory.
///
/// ```zig
/// const beam = @import("beam.zig");
///
/// fn give_me_ten_bytes() ![]u8 {
///   return beam.allocator.alloc(u8, 10);
/// }
/// ```
///
/// currently does not release memory that is resized.  For this behaviour, use
/// use `beam.general_purpose_allocator`.
///
/// not threadsafe.  for a threadsafe allocator, use `beam.general_purpose_allocator`
pub const allocator = raw_beam_allocator;

pub const MAX_ALIGN = 8;

const raw_beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &raw_beam_allocator_vtable,
};
const raw_beam_allocator_vtable = Allocator.VTable{
    .alloc = raw_beam_alloc,
    .resize = raw_beam_resize,
    .free = raw_beam_free,
};

fn raw_beam_alloc(
    _: *anyopaque,
    len: usize,
    ptr_align: u29,
    _: u29,
    _: usize,
) Allocator.Error![]u8 {
  if (ptr_align > MAX_ALIGN) { return error.OutOfMemory; }
  const ptr = e.enif_alloc(len) orelse return error.OutOfMemory;
  return @ptrCast([*]u8, ptr)[0..len];
}

fn raw_beam_resize(
    _: *anyopaque,
    buf: []u8,
    _: u29,
    new_len: usize,
    _: u29,
    _: usize,
) ?usize {
  if (new_len == 0) {
    e.enif_free(buf.ptr);
    return 0;
  }
  if (new_len <= buf.len) {
    return new_len;
  }
  // Is this the right thing to do???
  return null;
}

fn raw_beam_free(
    _: *anyopaque,
    buf: []u8,
    _: u29,
    _: usize,
) void {
    e.enif_free(buf.ptr);
}

/// !value
/// provides a BEAM allocator that can perform allocations with greater
/// alignment than the machine word.  Note that this comes at the cost
/// of some memory to store important metadata.
///
/// currently does not release memory that is resized.  For this behaviour
/// use `beam.general_purpose_allocator`.
///
/// not threadsafe.  for a threadsafe allocator, use `beam.general_purpose_allocator`
pub const large_allocator = large_beam_allocator;

const large_beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &large_beam_allocator_vtable,
};
const large_beam_allocator_vtable = Allocator.VTable{
    .alloc = large_beam_alloc,
    .resize = large_beam_resize,
    .free = Allocator.NoOpFree(anyopaque).noOpFree,
};

fn large_beam_alloc(_: *anyopaque, len: usize, alignment: u29, len_align: u29, return_address: usize) error{OutOfMemory}![]u8 {
    var ptr = try alignedAlloc(len, alignment, len_align, return_address);
    if (len_align == 0) {
        return ptr[0..len];
    }
    return ptr[0..std.mem.alignBackwardAnyAlign(len, len_align)];
}

fn large_beam_resize(
    _: *anyopaque,
    buf: []u8,
    buf_align: u29,
    new_len: usize,
    len_align: u29,
    _: usize,
) ?usize {
  if (new_len > buf.len) { return null; }
  if (new_len == 0) { return alignedFree(buf, buf_align); }
  if (len_align == 0) { return new_len; }
  return std.mem.alignBackwardAnyAlign(new_len, len_align);
}

fn alignedAlloc(len: usize, alignment: u29, _: u29, _: usize) ![*]u8 {
  var safe_len = safeLen(len, alignment);
  var alloc_slice: []u8 = try allocator.allocAdvanced(u8, MAX_ALIGN, safe_len, std.mem.Allocator.Exact.exact);

  const unaligned_addr = @ptrToInt(alloc_slice.ptr);
  const aligned_addr = reAlign(unaligned_addr, alignment);

  getPtrPtr(aligned_addr).* = unaligned_addr;
  return aligned_addr;
}

fn alignedFree(buf: []u8, alignment: u29) usize {
  var ptr = getPtrPtr(buf.ptr).*;
  allocator.free(@intToPtr([*]u8, ptr)[0..safeLen(buf.len, alignment)]);
  return 0;
}

fn reAlign(unaligned_addr: usize, alignment: u29) [*]u8 {
  return @intToPtr(
    [*]u8,
    std.mem.alignForward(
      unaligned_addr + @sizeOf(usize),
      alignment));
}

fn safeLen(len: usize, alignment: u29) usize {
  return len + alignment - @sizeOf(usize) + MAX_ALIGN;
}

fn getPtrPtr(aligned_ptr: [*]u8) *usize {
  return @intToPtr(*usize, @ptrToInt(aligned_ptr) - @sizeOf(usize));
}

/// !value
/// wraps the zig GeneralPurposeAllocator into the standard BEAM allocator.
var general_purpose_allocator_instance = std.heap.GeneralPurposeAllocator(
.{.thread_safe = true}) {
  .backing_allocator = large_allocator,
};

pub var general_purpose_allocator = general_purpose_allocator_instance.allocator();
