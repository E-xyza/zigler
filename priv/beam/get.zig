const beam = @import("beam.zig");
const e = @import("erl_nif.zig");
const std = @import("std");

pub fn get(comptime T: type, env: beam.env, src: beam.term) !T {
    // passthrough on beam.env and beam.term
    if (T == beam.term) return src;

    switch (@typeInfo(T)) {
        .Int => return get_int(T, env, src),
        else => @panic("unknown type encountered"),
    }
}

pub fn get_int(comptime T: type, env: beam.env, src: beam.term) !T {
    const int = @typeInfo(T).Int;
    switch (int.signedness) {
        .signed => switch (int.bits) {
            // TODO: make sure it's nil.
            0 => return @as(u0, void),
            1...32 => {
                var result: c_int = 0;
                // TODO: check to make sure this doesn't get weird
                _ = e.enif_get_int(env, src.v, &result);
                return lowerInt(T, result);
            },
            33...64 => {
                var result: i64 = 0;
                _ = e.enif_get_int64(env, src.v, &result);
                return lowerInt(T, result);
            },
            else => {
                unreachable;
            },
        },
        .unsigned => switch (int.bits) {
            // TODO: make sure it's nil.
            0 => return @as(u0, void),
            1...32 => {
                var result: c_uint = 0;
                // TODO: check to make sure this doesn't get weird
                _ = e.enif_get_uint(env, src.v, &result);
                return lowerInt(T, result);
            },
            33...64 => {
                var result: u64 = 0;
                _ = e.enif_get_uint64(env, src.v, &result);
                return lowerInt(T, result);
            },
            else => {
                // for integers bigger than 64-bytes the number
                // is imported as a binary.
                const Bigger = std.meta.Int(.unsigned, comptime try std.math.ceilPowerOfTwo(u16, int.bits));
                const bytes = @sizeOf(Bigger);

                // TODO: punt to get_binary
                var result: e.ErlNifBinary = undefined;
                // TODO: do things to check these.
                _ = e.enif_is_binary(env, src.v);
                _ = e.enif_inspect_binary(env, src.v, &result);

                var buf: Bigger = 0;
                std.mem.copy(u8, @ptrCast([*]u8, &buf)[0..bytes], result.data[0..bytes]);

                return @intCast(T, buf);
            },
        },
    }
    // for now, panic.
    unreachable;
}

inline fn lowerInt(comptime T: type, result: anytype) !T {
    // safety check this.  Consider making this able to be disabled.
    if (result <= std.math.maxInt(T)) {
        return @intCast(T, result);
    }
    unreachable; // panic, for now.
}
