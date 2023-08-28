///////////////////////////////////////////////////////////////////////////////
// BEAM allocator definitions
///////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const e = @import("erl_nif");

const Allocator = std.mem.Allocator;

pub const MAX_ALIGN = 8;

pub const raw_allocator = Allocator{
    .ptr = undefined,
    .vtable = &raw_beam_allocator_vtable,
};

const raw_beam_allocator_vtable = Allocator.VTable{
    .alloc = raw_beam_alloc,
    .resize = raw_beam_resize,
    .free = raw_beam_free,
};

pub var general_purpose_allocator_instance = make_general_purpose_allocator_instance();
pub const general_purpose_allocator = general_purpose_allocator_instance.allocator();

fn raw_beam_alloc(
    _: *anyopaque,
    len: usize,
    ptr_align: u8,
    _: usize,
) ?[*]u8 {
    if (ptr_align > MAX_ALIGN) return null;
    const ptr = e.enif_alloc(len) orelse return null;
    return @as([*]u8, @ptrCast(ptr));
}

fn raw_beam_resize(
    _: *anyopaque,
    buf: []u8,
    _: u8,
    new_len: usize,
    _: usize,
) bool {
    if (new_len <= buf.len) return true;
    if (new_len == 0) {
        e.enif_free(buf.ptr);
        return true;
    }
    // We are never able to increase the size of a pointer.
    return false;
}

fn raw_beam_free(
    _: *anyopaque,
    buf: []u8,
    _: u8,
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

fn large_beam_alloc(_: *anyopaque, len: usize, alignment: u8, _: usize) ?[*]u8 {
    var safe_len = safeLen(len, alignment);
    var alloc_slice: []u8 = raw_allocator.allocWithOptions(u8, safe_len, MAX_ALIGN, null) catch return null;

    const unaligned_addr = @intFromPtr(alloc_slice.ptr);
    const aligned_addr = reAlign(unaligned_addr, alignment);

    getPtrPtr(aligned_addr).* = unaligned_addr;
    return aligned_addr;
}

fn large_beam_resize(
    _: *anyopaque,
    buf: []u8,
    alignment: u8,
    new_len: usize,
    _: usize
) bool {
    if (new_len <= buf.len) return true;
    if (new_len == 0) return alignedFree(buf, alignment);
    return false;
}

fn large_beam_free(_: *anyopaque, buf: []u8, alignment: u8, _: usize) void {
    _ = alignedFree(buf, alignment);
}

fn alignedFree(buf: []u8, alignment: u8) bool {
    const ptr = getPtrPtr(buf.ptr).*;
    const slice = @as([*]u8, @ptrFromInt(ptr));
    return raw_allocator.free(slice[0..safeLen(buf.len, alignment)]);
}

fn reAlign(unaligned_addr: usize, alignment: u8) usize {
    return std.mem.alignForwardLog2(unaligned_addr + @sizeOf(usize), alignment);
}

fn safeLen(len: usize, alignment: u8) usize {
    return len + alignment - @sizeOf(usize) + MAX_ALIGN;
}

fn getPtrPtr(aligned_ptr: [*]u8) *usize {
    return @as(*usize, @ptrFromInt(@intFromPtr(aligned_ptr) - @sizeOf(usize)));
}

const BeamGpa = std.heap.GeneralPurposeAllocator(.{ .thread_safe = true });

pub fn make_general_purpose_allocator_instance() BeamGpa {
    return BeamGpa{ .backing_allocator = large_allocator };
}
