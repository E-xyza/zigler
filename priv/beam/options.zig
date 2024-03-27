/// common options utilities functions:
const std = @import("std");
const beam = @import("beam.zig");

pub inline fn allocator(opts: anytype) std.mem.Allocator {
    return if (@hasField(@TypeOf(opts), "allocator")) opts.allocator else beam.context.allocator;
}

pub inline fn env(opts: anytype) beam.env {
    return if (@hasField(@TypeOf(opts), "env")) opts.env else beam.context.env;
}

pub inline fn should_keep(opts: anytype) bool {
    return if (@hasField(@TypeOf(opts), "keep")) opts.keep else true;
}

pub inline fn should_cleanup(opts: anytype) bool {
    return if (@hasField(@TypeOf(opts), "cleanup")) opts.cleanup else true;
}

pub inline fn should_clear(opts: anytype) bool {
    return if (@hasField(@TypeOf(opts), "clear")) opts.clear else false;
}

pub const OutputType = enum { default, list, binary };

pub inline fn output(opts: anytype) OutputType {
    return if (@hasField(@TypeOf(opts), "output")) opts.output else .default;
}
