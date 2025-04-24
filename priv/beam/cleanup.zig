const std = @import("std");
const beam = @import("beam.zig");
const options = @import("options.zig");
const resource = @import("resource.zig");

const CPointerTags = enum { One, Many };
pub fn CPointerCleanup(comptime T: type) type {
    return ?union(CPointerTags) { One: *T, Many: []T };
}

fn needs_cleanup(comptime T: type, comptime should: bool) bool {
    return if (should) switch (@typeInfo(T)) {
        .pointer => true,
        .optional => |optional| needs_cleanup(optional.child, should),
        .@"struct" => struct_needs_cleanup(T, should),
        else => false,
    } else false;
}

fn struct_needs_cleanup(comptime T: type, comptime should: bool) bool {
    inline for (@typeInfo(T).@"struct".fields) |field| {
        if (needs_cleanup(field.type, should)) return true;
    }
    return false;
}

pub fn cleanup(what: anytype, opts: anytype) void {
    const T = @TypeOf(what);
    // TODO: this should work  via options.should_cleanup but somehow the
    // comptime-ness of this extracted field is not identified.
    const should = if (@hasField(@TypeOf(opts), "cleanup")) opts.cleanup else true;

    // TODO: eliminate this following dead code that forces comptime-ness:
    comptime var c = needs_cleanup(T, should);
    c = c and true;
    if (c) {
        switch (@typeInfo(T)) {
            .pointer => {
                cleanup_pointer(what, opts);
            },
            .optional => {
                if (what) |pointer| {
                    cleanup(pointer, opts);
                }
            },
            .@"struct" => |s| {
                if (resource.MaybeUnwrap(s)) |_| {
                    what.release();
                } else {
                    cleanup_struct(what, opts);
                }
            },
            else => {},
        }
    }
}

fn cleanup_pointer(ptr: anytype, opts: anytype) void {
    const info = @typeInfo(@TypeOf(ptr)).pointer;
    if (info.is_const) return;
    switch (info.size) {
        .one => {
            // TODO: more detailed cleanup.
            options.allocator(opts).destroy(ptr);
        },
        .slice => {
            for (ptr) |item| {
                cleanup(item, opts);
            }
            if (info.sentinel_ptr) |_| {
                options.allocator(opts).free(@as([]u8, @ptrCast(ptr)));
            } else {
                options.allocator(opts).free(ptr);
            }
        },
        .many, .c => {
            // cleaning up content can be done by specifying either a size parameter in the
            // cleanup options, or by specifying a cleanup function.  It can also be explictly
            // ignored by specifying a null cleanup function.  This function should take
            // pointer, plus the selfsame options tuple.  Specifying a size assumes that
            // the underlying memory was created using a slice operation.

            if (!@hasField(@TypeOf(opts), "size")) {
                @compileError("C or Many pointer cleanup requires size, or a function.");
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
    const info = @typeInfo(@TypeOf(s)).@"struct";

    inline for (info.fields) |field| {
        cleanup(@field(s, field.name), opts);
    }
}
