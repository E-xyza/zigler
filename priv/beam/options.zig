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

pub inline fn size(opts: anytype) ?usize {
    return if (@hasField(@TypeOf(opts), "size")) opts.size else null;
}

pub inline fn length(opts: anytype) ?usize {
    if (@hasField(@TypeOf(opts), "length")) {
        switch (@typeInfo(@TypeOf(opts.length))) {
            .comptime_int => return opts.length,
            .int => return opts.length,
            else => @compileError("length must be a comptime_int or a usize"),
        }
    } else return null;
}

pub inline fn sentinel_ptr(T: type, opts: anytype) ?*const T {
    if (@hasField(@TypeOf(opts), "sentinel")) {
        return @as(?*const T, &opts.sentinel);
    } else return null;
}

pub const OutputType = enum { default, list, binary, integer, map };

pub inline fn output(opts: anytype) OutputType {
    comptime { // NB: it's not entirely obvious why this has to be forced into the comptime scope!
        if (!@hasField(@TypeOf(opts), "as")) return .default;

        switch (@typeInfo(@TypeOf(opts.as))) {
            .enum_literal => {
                const tag = @tagName(opts.as);
                if (std.mem.eql(u8, tag, "default")) return .default;
                if (std.mem.eql(u8, tag, "list")) return .list;
                if (std.mem.eql(u8, tag, "binary")) return .binary;
                if (std.mem.eql(u8, tag, "integer")) return .integer;
                if (std.mem.eql(u8, tag, "map")) return .map;
                @compileError("invalid `as` type, must be default list, binary, integer, or map");
            },
            .@"struct" => |S| {
                for (S.fields) |field| {
                    if (std.mem.eql(u8, field.name, "list")) {
                        return .list;
                    }
                    if (std.mem.eql(u8, field.name, "map")) {
                        return .map;
                    }
                }
                @compileError("invalid `as` Struct, must have a field named `list` or `map`");
            },
            else => @compileError("invalid `as` type, must be an EnumLiteral or a Struct"),
        }
    }
}

pub fn assert_default(comptime T: type, opts: anytype) void {
    if (@hasField(@TypeOf(opts), "as")) {
        if (opts.as != .default) {
            const msg = std.fmt.comptimePrint("the 'as' field for the type {} must be `default`", .{T});
            @compileError(msg);
        }
    }
}

fn ListChildOf(T: type) type {
    switch (@typeInfo(T)) {
        .enum_literal => {
            return @TypeOf(.default);
        },
        .@"struct" => |S| {
            inline for (S.fields) |field| {
                if (std.mem.eql(u8, field.name, "list")) return field.type;
            }
            @compileError("list_child is only callable from a list declaration");
        },
        else => @compileError("the as field must be an enum literal or a tuple."),
    }
}

pub fn list_child(list_as: anytype) ListChildOf(@TypeOf(list_as)) {
    const T = @TypeOf(list_as);
    switch (@typeInfo(T)) {
        .enum_literal => return .default,
        .@"struct" => return list_as.list,
        else => unreachable,
    }
}

fn MapChildOf(T: type, comptime name: []const u8) type {
    switch (@typeInfo(T)) {
        .enum_literal => {
            return @TypeOf(.default);
        },
        .@"struct" => |S| {
            if (!@hasField(T, name)) return @TypeOf(.default);
            inline for (S.fields) |field| {
                if (std.mem.eql(u8, field.name, "map")) {
                    const M = field.type;
                    switch (@typeInfo(M)) {
                        .@"struct" => |MT| {
                            inline for (MT.fields) |mfield| {
                                if (std.mem.eql(u8, mfield.name, name)) return mfield.type;
                            }
                            const error_msg = std.fmt.comptimePrint("field '{s}' not found in target struct", .{name});
                            @compileError(error_msg);
                        },
                        else => @compileError("map as definition needs to be a struct"),
                    }
                    @compileError("map as definition needs to be a keyword list");
                }
            }
            @compileError("list_child is only callable from a map declaration");
        },
        else => @compileError("the as field must be an enum literal or a tuple."),
    }
}

pub fn map_child(map_as: anytype, comptime name: []const u8) MapChildOf(@TypeOf(map_as), name) {
    const T = @TypeOf(map_as);
    switch (@typeInfo(T)) {
        .enum_literal => return .default,
        .@"struct" => {
            const M = @TypeOf(map_as.map);
            if (@hasField(M, name)) return @field(map_as.map, name);
            return .default;
        },
        else => unreachable,
    }
}
