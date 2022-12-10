const std = @import("std");
const beam = @import("beam.zig");

fn allocator(opts: anytype) std.mem.Allocator {
    const T = @TypeOf(opts);
    if (@hasField(T, "allocator")) { return opts.allocator; }
    return beam.allocator;
}

pub fn cleanup(what: anytype, opts: anytype) void {
    const T = @TypeOf(what);
    switch (@typeInfo(T)) {
        .Pointer => {
            cleanup_pointer(T, what, opts);
        },
        else => return
    }
}

fn cleanup_pointer(comptime T: type, ptr: anytype, opts: anytype) void {
    switch (@typeInfo(T).Pointer.size) {
        .Slice => {
            allocator(opts).free(ptr);
        },
        else => @compileError("unimplemented")
    }
}