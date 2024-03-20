const std = @import("std");
const beam = @import("beam.zig");
//const resource = @import("resource.zig");

const CPointerTags = enum { One, Many };
pub fn CPointerCleanup(comptime T: type) type {
    return ?union(CPointerTags) { One: *T, Many: []T };
}

pub fn CleanupArtifact(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Pointer => PointerCleanupArtifact(T),
        else => @compileError("this type doesn't need an artifact"),
    };
}

fn PointerCleanupArtifact(comptime T: type) type {
    const info = @typeInfo(T);
    return switch (info.size) {
        .C => CPointerCleanup(info.child),
        else => @compileError("this type doesn't need an artifact"),
    };
}

fn allocator(opts: anytype) std.mem.Allocator {
    const T = @TypeOf(opts);
    if (@hasField(T, "allocator")) {
        return opts.allocator;
    }
    return beam.allocator;
}

pub fn cleanup(what: anytype, opts: anytype) void {
    const T = @TypeOf(what);
    switch (@typeInfo(T)) {
        .Pointer => {
            cleanup_pointer(what, opts);
        },
        .Optional => {
            if (what) |pointer| {
                cleanup(pointer, opts);
            }
        },
        .Struct => |_| {
            //if (resource.MaybeUnwrap(s)) |_| {
            //    cleanup_resource(what, opts);
            //}
        },
        else => {},
    }
}

fn cleanup_pointer(ptr: anytype, opts: anytype) void {
    const Opts = @TypeOf(opts);
    const T = @TypeOf(ptr);
    const info = @typeInfo(T).Pointer;
    if (info.is_const) return;

    switch (info.size) {
        .One => {
            // TODO: more detailed cleanup.
            allocator(opts).destroy(ptr);
        },
        .Slice => {
            if (info.sentinel) |_| {
                allocator(opts).free(@as([]u8, @ptrCast(ptr)));
            } else {
                allocator(opts).free(ptr);
            }
        },
        .Many, .C => {
            // cleaning up content can be done by specifying either a size parameter in the
            // cleanup options, or by specifying a cleanup function.  It can also be explictly
            // ignored by specifying a null cleanup function.  This function should take
            // pointer, plus the selfsame options tuple.  Specifying a size assumes that
            // the underlying memory was created using a slice operation.
            if (@hasField(Opts, "size")) {
                if (info.is_allowzero) {
                    if (ptr) |_| {
                        const underlying_slice = ptr[0..opts.size];
                        allocator(opts).free(underlying_slice);
                    }
                } else {
                    const underlying_slice = ptr[0..opts.size];
                    allocator(opts).free(underlying_slice);
                }
            } else if (@hasField(Opts, "cleanup")) {
                maybe_cleanup_pointer_with_function(ptr, opts);
            } else {
                @compileError("C or Many pointer cleanup requires size, or a function.  If you would like to ignore cleanup, specify a null function.");
            }
        },
    }
}

fn cleanup_resource(res: anytype, opts: anytype) void {
    if (should_cleanup(opts)) {
        res.release();
    }
}

fn maybe_cleanup_pointer_with_function(ptr: anytype, opts: anytype) void {
    const T = @TypeOf(opts.cleanup);
    switch (@typeInfo(T)) {
        .Null => return,
        .Fn => opts.cleanup(ptr, opts),
    }
}

fn should_cleanup(opts: anytype) bool {
    return if (@hasField(@TypeOf(opts), "cleanup")) opts.cleanup else true;
}
