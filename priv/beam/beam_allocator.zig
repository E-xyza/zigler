//! basic beam allocator wrapped in the zig allocator interface.
//! no initialization provided, since that the entire thing is
//! initialized as a part of the BEAM virtual machine.

const std = @import("std");
const e = @import("erl_nif.zig");
const Allocator = std.mem.Allocator;
const alignment = u29;

pub const MAX_ALIGN = 8;

pub const beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &beam_allocator_vtable,
};

const beam_allocator_vtable = Allocator.VTable{
    .alloc = BeamAllocator.alloc,
    .resize = BeamAllocator.resize,
    .free = BeamAllocator.free,
};

const BeamAllocator = struct {
    fn alloc(
        _self: *anyopaque,
        len: usize,
        ptr_align: alignment,
        _len_align: alignment,
        _ret_addr: usize) ![]u8 {

        _ = _self;
        _ = _len_align;
        _ = _ret_addr;

        if (ptr_align > MAX_ALIGN) { return error.OutOfMemory; }
        const ptr = e.enif_alloc(len) orelse return error.OutOfMemory;
        return @ptrCast([*]u8, ptr)[0..len];
    }

    fn resize(
        _self: *anyopaque,
        buf: []u8,
        _buf_align: alignment,
        new_len: usize,
        _len_align: alignment,
        _ret_addr: usize
    ) ?usize {
        _ = _self;
        _ = _buf_align;
        _ = _len_align;
        _ = _ret_addr;

        // only allows sizing DOWN the memory.  Doesn't allow sizing UP.

        if (new_len == 0) {
          e.enif_free(buf.ptr);
          return 0;
        }
        if (new_len <= buf.len) {
          return new_len;
        }
        return null;
    }

    fn free(
        _self: *anyopaque,
        buf: []u8,
        _buf_align: alignment,
        _ret_addr: usize) void {

        _ = _self;
        _ = _buf_align;
        _ = _ret_addr;

        e.enif_free(buf.ptr);
    }
};

pub const large_beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &large_beam_allocator_vtable,
};

const large_beam_allocator_vtable = Allocator.VTable{
    .alloc = LargeBeamAllocator.alloc,
    .resize = LargeBeamAllocator.resize,
    .free = LargeBeamAllocator.free,
};

const LargeBeamAllocator = struct {
    fn alloc(
        _self: *anyopaque,
        len: usize,
        ptr_align: alignment,
        len_align: alignment,
        ret_addr: usize) ![]u8 {

        _ = _self;

        var ptr = try alignedAlloc(len, ptr_align, len_align, ret_addr);
        if (len_align == 0) { return ptr[0..len]; }
        return ptr[0..std.mem.alignBackwardAnyAlign(len, len_align)];
    }

    fn alignedAlloc(
        len: usize,
        ptr_align: alignment,
        _len_align: alignment,
        _return_address: usize) ![*]u8 {

        _ = _len_align;
        _ = _return_address;

        var safe_len = safeLen(len, ptr_align);
        var alloc_slice: []u8 = try beam_allocator.allocAdvanced(
          u8, MAX_ALIGN, safe_len, std.mem.Allocator.Exact.exact);

        const unaligned_addr = @ptrToInt(alloc_slice.ptr);
        const aligned_addr = reAlign(unaligned_addr, ptr_align);

        getPtrPtr(aligned_addr).* = unaligned_addr;
        return aligned_addr;
    }

    fn safeLen(len: usize, ptr_align: alignment) usize {
        return len + ptr_align - @sizeOf(usize) + MAX_ALIGN;
    }

    fn reAlign(unaligned_addr: usize, ptr_align: alignment) [*]u8 {
        return @intToPtr(
            [*]u8,
            std.mem.alignForward(
                unaligned_addr + @sizeOf(usize),
                ptr_align));
    }

    fn alignedFree(
        buf: []u8,
        ptr_align: alignment) usize {

        var ptr = getPtrPtr(buf.ptr).*;
        beam_allocator.free(@intToPtr([*]u8, ptr)[0..safeLen(buf.len, ptr_align)]);
        return 0;
    }

    fn getPtrPtr(aligned_ptr: [*]u8) *usize {
        return @intToPtr(*usize, @ptrToInt(aligned_ptr) - @sizeOf(usize));
    }

    fn resize(
        _self: *anyopaque,
        buf: []u8,
        buf_align: alignment,
        new_len: usize,
        len_align: alignment,
        _ret_addr: usize
    ) ?usize {
        // discards
        _ = _self;
        _ = _ret_addr;

        if (new_len > buf.len) { return null; }
        if (new_len == 0) { return alignedFree(buf, buf_align); }
        if (len_align == 0) { return new_len; }
        return std.mem.alignBackwardAnyAlign(new_len, len_align);
    }

    fn free(
        _self: *anyopaque,
        buf: []u8,
        _buf_align: alignment,
        _ret_addr: usize) void {

        _ = _self;
        _ = _buf_align;
        _ = _ret_addr;

        e.enif_free(getPtrPtr(buf.ptr));
    }
};
