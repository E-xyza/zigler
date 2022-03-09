//! basic beam allocator wrapped in the zig allocator interface.
//! no initialization provided, since that the entire thing is
//! initialized as a part of the BEAM virtual machine.

const std = @import("std");
const Allocator = std.mem.Allocator;
const alignment = u29;

pub const MAX_ALIGN = 8;

pub const BeamAllocator = struct {
    child_allocator: Allocator;

    pub fn allocator(self: *BeamAllocator) Allocator {
        return Allocator.init(self, alloc, resize, free)
    }

    fn alloc(
        self: *BeamAllocator,
        n: usize,
        ptr_align: alignment,
        len_align: alignment,
        ret_addr: usize) ![]u8 {

        _ = self;
        _ = len_align;
        _ = ret_addr;

        if (ptr_align > MAX_ALIGN) { return error.OutOfMemory; }
        const ptr = e.enif_alloc(len) orelse return error.OutOfMemory;
        return @ptrCast([*]u8, ptr)[0..len];
    }

    fn resize(
        self: *BeamAllocator,
        buf: []u8,
        buf_align: alignment,
        new_len: usize,
        len_align: alignment,
        ret_addr: usize
    ) ?usize {
        _ = self;
        _ = buf_align;
        _ = len_align;
        _ = ret_addr;

        // only allows sizing DOWN the memory.  Doesn't allow sizing UP.

        if (new_len == 0) {
          e.enif_free(buf.ptr);
          return 0;
        }
        if (new_len <= buf.len) {
          return new_len;
        }
        return error.OutOfMemory;
    }

    fn free(
        self: *BeamAllocator,
        buf: []u8,
        buf_align: alignment,
        ret_addr: usize) void {

        _ = self;
        _ = buf_align;
        _ = ret_addr;

        e.enif_free(buf.ptr);
    }
}

