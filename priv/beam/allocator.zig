///////////////////////////////////////////////////////////////////////////////
// BEAM allocator definitions
///////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const e = @import("erl_nif.zig");

const Allocator = std.mem.Allocator;

pub const MAX_ALIGN = 8;

pub const raw_beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &raw_beam_allocator_vtable,
};

const raw_beam_allocator_vtable = Allocator.VTable{
    .alloc = raw_beam_alloc,
    .resize = raw_beam_resize,
    .free = raw_beam_free,
};

var general_purpose_allocator_instance = make_general_purpose_allocator_instance();
pub const general_purpose_allocator = general_purpose_allocator_instance.allocator();

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

pub const large_allocator = large_beam_allocator;

const large_beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &large_beam_allocator_vtable,
};

const large_beam_allocator_vtable = Allocator.VTable{
    .alloc = large_beam_alloc,
    .resize = large_beam_resize,
    .free = large_beam_free,
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

fn large_beam_free(_: *anyopaque, buf: []u8, buf_align: u29, _: usize) void {
  _ = alignedFree(buf, buf_align);
}

fn alignedAlloc(len: usize, alignment: u29, _: u29, _: usize) ![*]u8 {
  var safe_len = safeLen(len, alignment);
  var alloc_slice: []u8 = try raw_beam_allocator.allocAdvanced(u8, MAX_ALIGN, safe_len, std.mem.Allocator.Exact.exact);

  const unaligned_addr = @ptrToInt(alloc_slice.ptr);
  const aligned_addr = reAlign(unaligned_addr, alignment);

  getPtrPtr(aligned_addr).* = unaligned_addr;
  return aligned_addr;
}

fn alignedFree(buf: []u8, alignment: u29) usize {
  var ptr = getPtrPtr(buf.ptr).*;
  raw_beam_allocator.free(@intToPtr([*]u8, ptr)[0..safeLen(buf.len, alignment)]);
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

const BeamGpa = std.heap.GeneralPurposeAllocator(.{.thread_safe = true});

pub fn make_general_purpose_allocator_instance() BeamGpa {
  return BeamGpa{.backing_allocator = large_allocator};
}
