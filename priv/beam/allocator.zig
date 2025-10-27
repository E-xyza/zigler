///////////////////////////////////////////////////////////////////////////////
// BEAM allocator definitions
///////////////////////////////////////////////////////////////////////////////

const std = @import("std");
const e = @import("erl_nif");
const assert = std.debug.assert;

const Allocator = std.mem.Allocator;

pub const MAX_ALIGN = 8;

/////////////////////////////////////////////////////////////////////////////
// Beam allocator

/// namespace for BEAM allocator that has access to alignment
const BeamAllocator = struct {
    fn alloc(_: *anyopaque, len: usize, alignment: std.mem.Alignment, return_address: usize) ?[*]u8 {
        _ = return_address;
        assert(len > 0);
        return aligned_alloc(len, alignment);
    }

    fn resize(
        _: *anyopaque,
        buf: []u8,
        alignment: std.mem.Alignment,
        new_len: usize,
        return_address: usize,
    ) bool {
        _ = alignment;
        _ = return_address;
        // we can shrink buffers but we can't grow them.
        return new_len < buf.len;
    }

    fn remap(context: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) ?[*]u8 {
        // can't use realloc directly because it might not respect alignment.
        return if (resize(context, memory, alignment, new_len, return_address)) memory.ptr else null;
    }

    fn free(_: *anyopaque, memory: []u8, alignment: std.mem.Alignment, return_address: usize) void {
        _ = alignment;
        _ = return_address;
        aligned_free(memory.ptr);
    }

    // implementation of aligned operations

    fn aligned_alloc(len: usize, alignment: std.mem.Alignment) ?[*]u8 {
        const alignment_bytes = alignment.toByteUnits();

        const unaligned_ptr = @as([*]u8, @ptrCast(e.enif_alloc(len + alignment_bytes - 1 + @sizeOf(usize)) orelse return null));
        const unaligned_addr = @intFromPtr(unaligned_ptr);
        const aligned_addr = std.mem.alignForward(usize, unaligned_addr + @sizeOf(usize), alignment_bytes);
        const aligned_ptr = unaligned_ptr + (aligned_addr - unaligned_addr);
        get_header(aligned_ptr).* = unaligned_ptr;
        return aligned_ptr;
    }

    fn aligned_free(ptr: [*]u8) void {
        const unaligned_ptr = get_header(ptr).*;
        e.enif_free(unaligned_ptr);
    }

    fn get_header(ptr: [*]u8) *[*]u8 {
        return @alignCast(@ptrCast(ptr - @sizeOf(usize)));
    }

    const vtable: std.mem.Allocator.VTable = .{
        .alloc = alloc,
        .resize = resize,
        .remap = remap,
        .free = free,
    };
};

pub const beam_allocator: std.mem.Allocator = .{ .ptr = undefined, .vtable = &BeamAllocator.vtable };

/////////////////////////////////////////////////////////////////////////////
// Raw allocator

/// namespace for BEAM allocator that is restricted to 8-byte alignment
pub const raw_beam_allocator: std.mem.Allocator = .{
    .ptr = undefined,
    .vtable = &raw_beam_allocator_vtable,
};

const raw_beam_allocator_vtable = std.mem.Allocator.VTable{
    .alloc = raw_alloc,
    .resize = raw_resize,
    .remap = raw_remap,
    .free = raw_free,
};

fn raw_alloc(context: *anyopaque, len: usize, alignment: std.mem.Alignment, return_address: usize) ?[*]u8 {
    _ = context;
    _ = return_address;
    assert(alignment.compare(.lte, comptime .fromByteUnits(MAX_ALIGN)));
    assert(len > 0);
    return @ptrCast(e.enif_alloc(len));
}

fn raw_resize(context: *anyopaque, buf: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) bool {
    _ = context;
    _ = alignment;
    _ = buf;
    _ = new_len;
    _ = return_address;
    return false;
}

fn raw_remap(context: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, return_address: usize) ?[*]u8 {
    _ = context;
    _ = alignment;
    _ = return_address;
    return @ptrCast(e.enif_realloc(memory.ptr, new_len)); // can't remap with raw allocator
}

fn raw_free(context: *anyopaque, memory: []u8, alignment: std.mem.Alignment, return_address: usize) void {
    _ = context;
    _ = alignment;
    _ = return_address;
    e.enif_free(memory.ptr);
}

/////////////////////////////////////////////////////////////////////////////
// Debug allocator

const BeamDebugAllocator = std.heap.DebugAllocator(.{
    .thread_safe = true,
    // On Windows, stack trace collection in dynamically loaded libraries (NIFs) can cause
    // segfaults due to limitations in the debug info access. Disable stack traces on Windows.
    .stack_trace_frames = if (@import("builtin").os.tag == .windows) 0 else 8,
});

pub fn make_debug_allocator_instance() BeamDebugAllocator {
    return BeamDebugAllocator{ .backing_allocator = beam_allocator };
}

pub var debug_allocator_instance = make_debug_allocator_instance();
pub const debug_allocator = debug_allocator_instance.allocator();
