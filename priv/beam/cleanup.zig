const std = @import("std");
const beam = @import("beam.zig");
const options = @import("options.zig");
const resource = @import("resource.zig");

const CPointerTags = enum { One, Many };
pub fn CPointerCleanup(comptime T: type) type {
    return ?union(CPointerTags) { One: *T, Many: []T };
}

fn needs_cleanup(comptime T: type, opts: anytype) bool {
    if (!options.should_cleanup(opts)) return false;
    return switch (@typeInfo(T)) {
        .Pointer => true,
        .Optional => |optional| needs_cleanup(optional.child, opts),
        .Struct => struct_needs_cleanup(T, opts),
        else => false
    };
}

fn struct_needs_cleanup(comptime T: type, opts: anytype) bool {
    inline for (@typeInfo(T).Struct.fields) |field| {
        if (needs_cleanup(field.type, opts)) return true;
    }
    return false;
}

pub fn cleanup(what: anytype, opts: anytype) void {
    const T = @TypeOf(what);
    if (!needs_cleanup(T, opts)) return;

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
                what.release();
            } else {
                cleanup_struct(what, opts);
            }
        },
        else => {},
    }
}

fn cleanup_pointer(ptr: anytype, opts: anytype) void {
    const info = @typeInfo(@TypeOf(ptr)).Pointer;
    if (info.is_const) return;

    switch (info.size) {
        .One => {
            // TODO: more detailed cleanup.
            options.allocator(opts).destroy(ptr);
        },
        .Slice => {
            for (ptr) |item| { cleanup(item, opts); }
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

            if (!@hasField(@TypeOf(opts), "size")) {
                @compileError("C or Many pointer cleanup requires size, or a function.  If you would like to ignore cleanup, specify a null function.");
            }

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
            }
        },
    }
}

fn cleanup_struct(s: anytype, opts: anytype) void {
    const info = @typeInfo(@TypeOf(s)).Struct;

    inline for (info.fields) |field| {
        cleanup(@field(s, field.name), opts);
    }
}
