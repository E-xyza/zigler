const beam = @import("beam.zig");
const e = @import("erl_nif.zig");
const std = @import("std");

pub fn get(comptime T: type, env: beam.env, src: beam.term) !T {
    switch (@typeInfo(T)) {
        .Int => return get_int(T, env, src),
        else => @panic("unknown type encountered")
    }
}

pub fn get_int(comptime T: type, env: beam.env, src: beam.term) !T {
    const int = @typeInfo(T).Int;
    switch (int.signedness) {
        .signed =>
             if (int.bits <= 32) {
                var result: c_int = undefined;
                // TODO: check to make sure this doesn't get weird
                _ = e.enif_get_int(env, src, &result);
                return lower_int(T, result);
             } else if (int.bits <= 64) {
                var result: i64 = undefined;
                _ = e.enif_get_int64(env, src, &result);
                return lower_int(T, result);
             },
        .unsigned =>
            if (int.bits <= 31) {
                var result: c_int = undefined;
                _ = e.enif_get_int(env, src, &result);
                return lower_int(T, result);
            } else if (int.bits <= 63) {
                var result: i64 = undefined;
                _ = e.enif_get_int64(env, src, &result);
                return lower_int(T, result);
            } else if (int.bits == 64) {
                var result: i64 = undefined;
                _ = e.enif_get_int64(env, src, &result);
                return @bitCast(u64, result);
            }
            // we must do something different for 64-bit unsigned integers!
    }
    // for now, panic.
    unreachable;
}

fn lower_int(comptime T: type, result: anytype) !T {
    // safety check this.  Consider making this able to be disabled.
    if (result <= std.math.maxInt(T)) {
        return @intCast(T, result);
    }
    unreachable;  // panic, for now.
}