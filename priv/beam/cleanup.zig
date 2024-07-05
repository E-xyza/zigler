const std = @import("std");
const beam = @import("beam.zig");
const options = @import("options.zig");
const resource = @import("resource.zig");

const CPointerTags = enum { One, Many };
pub fn CPointerCleanup(comptime T: type) type {
    return ?union(CPointerTags) { One: *T, Many: []T };
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
        .Struct => |s| {
            if (resource.MaybeUnwrap(s)) |_| {
                cleanup_resource(what, opts);
            }
        },
        else => {},
    }
}

fn cleanup_pointer(ptr: anytype, opts: anytype) void {
    if (!options.should_cleanup(opts)) return;
    const T = @TypeOf(ptr);
    const info = @typeInfo(T).Pointer;
    if (info.is_const) return;

    switch (info.size) {
        .One => {
            // TODO: more detailed cleanup.
            options.allocator(opts).destroy(ptr);
        },
        .Slice => {
            if (info.sentinel) |_| {
                options.allocator(opts).free(@as([]u8, @ptrCast(ptr)));
            } else {
                options.allocator(opts).free(ptr);
            }
        },
        .Many, .C => {
            // cleaning up content can be done by specifying either a size parameter in the
            // cleanup options, or by specifying a cleanup function.  It can also be explictly
            // ignored by specifying a null cleanup function.  This function should take
            // pointer, plus the selfsame options tuple.  Specifying a size assumes that
            // the underlying memory was created using a slice operation.
            if (options.size(opts)) |size| {
                if (info.is_allowzero) {
                    if (ptr) |_| {
                        const underlying_slice = ptr[0..size];
                        options.allocator(opts).free(underlying_slice);
                    }
                } else {
                    const underlying_slice = ptr[0..size];
                    options.allocator(opts).free(underlying_slice);
                }
            } else {
                // TODO: custom cleanup function goes here.
                @compileError("C or Many pointer cleanup requires size, or a function.  If you would like to ignore cleanup, specify a null function.");
            }
        },
    }
}

fn cleanup_resource(res: anytype, opts: anytype) void {
    if (options.should_cleanup(opts)) {
        res.release();
    }
}
