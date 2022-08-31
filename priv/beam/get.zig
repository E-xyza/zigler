const beam = @import("beam.zig");
const e = @import("erl_nif.zig");
const std = @import("std");

pub const Error = error {
    nif_argument_type_error,
    nif_argument_range_error,
    nif_marshalling_error  // this should really not happen.
};

pub fn get(comptime T: type, env: beam.env, src: beam.term) !T {
    // passthrough on beam.env and beam.term
    if (T == beam.term) return src;

    switch (@typeInfo(T)) {
        .Int => return get_int(T, env, src),
        .Enum => return get_enum(T, env, src),
        else => @panic("unknown type encountered"),
    }
}

const c_int_size = @bitSizeOf(c_int);
const c_long_size = @bitSizeOf(c_long);
const i32_t = if (c_int_size == 32) c_int else if (c_long_size == 32) c_long;
const enif_get_i32 = if (c_int_size == 32) e.enif_get_int else if (c_long_size == 32) e.enif_get_long;
const u32_t = if (c_int_size == 32) c_uint else if (c_long_size == 32) c_ulong;
const enif_get_u32 = if (c_int_size == 32) e.enif_get_uint else if (c_long_size == 32) e.enif_get_ulong;

pub fn get_int(comptime T: type, env: beam.env, src: beam.term) !T {
    const int = @typeInfo(T).Int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            0...32 => {
                var result: i32_t = 0;

                if (src.term_type(env) != .integer) {
                    return Error.nif_argument_type_error;
                }

                if (enif_get_i32(env, src.v, &result) == 0) return Error.nif_argument_range_error;

                return lowerInt(T, result);
            },
            33...64 => {
                var result: i64 = 0;

                if (src.term_type(env) != .integer) {
                    return Error.nif_argument_type_error;
                }

                if (e.enif_get_int64(env, src.v, &result) == 0) return Error.nif_argument_range_error;

                return lowerInt(T, result);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(env, src.v, &result) == 0) return Error.nif_marshalling_error;


                var buf: Bigger = 0;
                std.mem.copy(u8, @ptrCast([*]u8, &buf)[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(Bigger, buf) < top_bit_count) return Error.nif_argument_range_error;

                return @intCast(T, buf);
            },
        },
        .unsigned => switch (int.bits) {
            0...32 => {
                var result: u32_t = 0;

                if (src.term_type(env) != .integer) {
                    return Error.nif_argument_type_error;
                }

                // TODO: check to make sure this doesn't get weird
                if (enif_get_u32(env, src.v, &result) == 0) return Error.nif_argument_range_error;

                return try lowerInt(T, result);
            },
            33...64 => {
                var result: u64 = 0;

                if (src.term_type(env) != .integer) {
                    return Error.nif_argument_type_error;
                }

                if (e.enif_get_uint64(env, src.v, &result) == 0) return Error.nif_argument_range_error;

                return try lowerInt(T, result);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                var result: e.ErlNifBinary = undefined;

                // This should fail if it's not a binary.  Note there isn't much we can do here because
                // it is *supposed* to be marshalled into the nif.
                if (e.enif_inspect_binary(env, src.v, &result) == 0) return Error.nif_marshalling_error;


                var buf: Bigger = 0;
                std.mem.copy(u8, @ptrCast([*]u8, &buf)[0..bytes], result.data[0..bytes]);
                // check to make sure that the top bits are all zeros.
                const top_bit_count = (bytes * 8 - int.bits);
                if (@clz(Bigger, buf) < top_bit_count) return Error.nif_argument_range_error;

                return @intCast(T, buf);
            },
        },
    }
    // for now, panic.
    unreachable;
}

inline fn lowerInt(comptime T: type, result: anytype) !T {
    const int = @typeInfo(T).Int;
    if (int.signedness == .signed) {
        if (result < std.math.minInt(T)) {
            return Error.nif_argument_range_error;
        }
    }

    if (result > std.math.maxInt(T)) {
        return Error.nif_argument_range_error;
    }

    return @intCast(T, result);
}

pub fn get_enum(comptime T: type, env: beam.env, src: beam.term) !T {
    const IntType = @typeInfo(T).Enum.tag_type;
    // there is really nothing we can do if it gets sent in as something not
    // an integer.
    if (src.term_type(env) != .integer) return Error.nif_marshalling_error;

    // retrieve it as the integer form.
    return @intToEnum(T, try get_int(IntType, env, src));
}