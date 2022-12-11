const std = @import("std");
const beam = @import("beam.zig");

const CPointerTags = enum{One, Many};
pub fn CPointerCleanup(comptime T: type) type {
    return ?union(CPointerTags) {
        One: *T,
        Many: []T
    };
}

pub fn CleanupArtifact(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Pointer => PointerCleanupArtifact(T),
        else => @compileError("this type doesn't need an artifact")
    };
}

fn PointerCleanupArtifact(comptime T: type) type {
    const info = @typeInfo(T);
    return switch(info.size) {
        .C => CPointerCleanup(info.child),
        else => @compileError("this type doesn't need an artifact")
    };
}
 
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
    const Opts = @TypeOf(opts);
    switch (@typeInfo(T).Pointer.size) {
        .One => {
            allocator(opts).destroy(ptr);
        },
        .Slice => {
            allocator(opts).free(ptr);
        },
        .Many, .C => {
            if (@hasDecl(Opts, "cleanup")) {
                opts.cleanup(ptr);
            }
        },
    }
}