//! generic expanding allocator for zig.  Allows you to easily wrap the BEAM
//! allocator in something which can then be used to do full-page allocations
//! (for example the General Purpose Allocator)

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const Self = @This();

allocator: Allocator = Allocator{
    .allocFn = alloc,
    .resizeFn = resize,
},

// these two values need to be set for this to work.
backing_allocator: *Allocator = undefined,
max_align: u29 = undefined,

///////////////////////////////////////////////////////////////////////////
// Allocator API Functions

fn alloc(
    allocator: *Allocator,
    len: usize,
    alignment: u29,
    len_align: u29,
    return_address: usize,
) error{OutOfMemory}![]u8 {
    const self = @fieldParentPtr(Self, "allocator", allocator);

    assert(len > 0);
    assert(std.math.isPowerOfTwo(alignment));

    if (alignment <= self.max_align) {
        return try self.backing_allocator.allocFn(self.backing_allocator, len, alignment, len_align, return_address);
    } else {
        var ptr = try alignedAlloc(self.backing_allocator, len, alignment, self.max_align, len_align, return_address);

        if (len_align == 0) {
            return ptr[0..len];
        }
        return ptr[0..mem.alignBackwardAnyAlign(len, len_align)];
    }
}

fn resize(
    allocator: *Allocator,
    buf: []u8,
    buf_align: u29,
    new_len: usize,
    len_align: u29,
    ret_addr: usize,
) Allocator.Error!usize {
    const self = @fieldParentPtr(Self, "allocator", allocator);

    if (buf_align <= self.max_align) {
        return try self.backing_allocator.resizeFn(self.backing_allocator, buf, buf_align, new_len, len_align, ret_addr);
    }

    var ptr = try alignedResize(self.backing_allocator, buf, self.max_align, new_len, len_align, ret_addr);

    if (len_align == 0) { return new_len; }

    return mem.alignBackwardAnyAlign(new_len, len_align);
}

///////////////////////////////////////////////////////////////////////////////
// helper functions

const header = struct {
    length: usize,
    unaligned: [*]u8,
};

fn alignedAlloc(allocator: *Allocator, len: usize, alignment: u29, max_align: u29, len_align: u29, return_address: usize) ![*]u8 {
    // Unsophisticated wrapper around parent allocator, overallocate to
    // account for alignment padding and store -2) the orignal malloc()'ed
    // pointer before the aligned address and -1) the original allocated size
    // before the aligned address.
    var true_len = trueLen(len, alignment);

    var alloc_slice: []u8 = try allocator.allocFn(allocator, true_len, max_align, len_align, return_address);

    var unaligned: [*]u8 = alloc_slice.ptr;

    const unaligned_addr = @ptrToInt(unaligned);
    const aligned_addr = mem.alignForward(unaligned_addr + @sizeOf(usize), alignment);
    var aligned_ptr = unaligned + (aligned_addr - unaligned_addr);

    var hdr = getHeader(aligned_ptr);

    hdr.unaligned = unaligned;
    hdr.length = true_len;

    return aligned_ptr;
}

fn alignedResize(allocator: *Allocator, buf: []u8, alignment: u29, new_len: usize, len_align: u29, ret_addr: usize) !usize {
    var hdr = getHeader(buf.ptr);
    var true_buf = trueBuf(buf);

    if (new_len == 0) {
        return try allocator.resizeFn(allocator, true_buf, alignment, 0, len_align, ret_addr);
    }

    var true_len = trueLen(new_len, alignment);
    var resize_slice = try allocator.resizeFn(allocator, true_buf, alignment, true_len, len_align, ret_addr);

    hdr.length = true_len;

    return resize_slice;
}

fn alignedFree(allocator: *Allocator, alloc_slice: []u8) void {
    var hdr = getHeader(alloc_slice.ptr);

    var slice = hdr.unaligned[0..hdr.length];
    allocator.resizeFn(slice, alignment, 0, );
}

fn getHeader(ptr: [*]u8) *header {
    return @intToPtr(*header, @ptrToInt(ptr) - @sizeOf(header));
}

fn trueBuf(pseudoBuf: []u8) []u8 {
    var hdr: *header = getHeader(pseudoBuf.ptr);
    return hdr.unaligned[0..hdr.length];
}

fn trueLen(len: usize, alignment: u29) usize {
    return len + alignment - 1 + @sizeOf(header);
}