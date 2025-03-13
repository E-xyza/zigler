///////////////////////////////////////////////////////////////////////////////
// BEAM allocator definitions
///////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const e = @import("erl_nif");

const Allocator = std.mem.Allocator;

pub const MAX_ALIGN = 8;

pub const allocator = Allocator{
    .ptr = undefined,
    .vtable = &raw_beam_allocator_vtable,
};

const raw_beam_allocator_vtable = Allocator.VTable{
    .alloc = raw_beam_alloc,
    .resize = raw_beam_resize,
    .free = raw_beam_free,
};

pub var debug_allocator_instance = make_debug_allocator_instance();
pub const debug_allocator = debug_allocator_instance.allocator();

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

fn getHeader(ptr: [*]u8) *[*]u8 {
    return @as(*[*]u8, @ptrFromInt(@intFromPtr(ptr) - @sizeOf(usize)));
}

fn alignedAlloc(len: usize, log2_align: u8) ?[*]u8 {
    const alignment = @as(usize, 1) << @as(Allocator.Log2Align, @intCast(log2_align));

    // Thin wrapper around regular e.enif_alloc, overallocate to account for
    // alignment padding and store the original malloc()'ed pointer before
    // the aligned address.
    const unaligned_ptr = @as([*]u8, @ptrCast(e.enif_alloc(len + alignment - 1 + @sizeOf(usize)) orelse return null));
    const unaligned_addr = @intFromPtr(unaligned_ptr);
    const aligned_addr = std.mem.alignForward(usize, unaligned_addr + @sizeOf(usize), alignment);
    const aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
    getHeader(aligned_ptr).* = unaligned_ptr;

    return aligned_ptr;
}

fn alignedFree(ptr: [*]u8) void {
    const unaligned_ptr = getHeader(ptr).*;
    e.enif_free(unaligned_ptr);
}

pub const wide_alignment_allocator = Allocator{
    .ptr = undefined,
    .vtable = &wide_alignment_allocator_vtable,
};

const wide_alignment_allocator_vtable = Allocator.VTable{
    .alloc = wide_align_alloc,
    .resize = wide_align_resize,
    .free = wide_align_free,
};

fn wide_align_alloc(
    _: *anyopaque,
    len: usize,
    log2_align: u8,
    return_address: usize,
) ?[*]u8 {
    _ = return_address;
    if (len <= 0) @panic("alloc: len must be greater than 0");
    return alignedAlloc(len, log2_align);
}

fn wide_align_resize(
    _: *anyopaque,
    buf: []u8,
    log2_buf_align: u8,
    new_len: usize,
    return_address: usize,
) bool {
    _ = log2_buf_align;
    _ = return_address;
    if (new_len <= buf.len) {
        return true;
    }
    return false;
}

fn wide_align_free(
    _: *anyopaque,
    buf: []u8,
    log2_buf_align: u8,
    return_address: usize,
) void {
    _ = log2_buf_align;
    _ = return_address;
    alignedFree(buf.ptr);
}

const BeamDebugAllocator = std.heap.DebugAllocator(.{ .thread_safe = false });

pub fn make_debug_allocator_instance() BeamDebugAllocator {
    return BeamDebugAllocator{ .backing_allocator = wide_alignment_allocator };
}
