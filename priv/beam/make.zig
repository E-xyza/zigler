const beam = @import("beam.zig");
const e = @import("erl_nif.zig");

pub fn make(env: beam.env, value: anytype) beam.term {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .Int => return make_int(env, value),
        else => @panic("unknown type encountered")
    }
}

fn make_int(env: beam.env, value: anytype) beam.term {
    const int = @typeInfo(@TypeOf(value)).Int;
    switch (int.signedness) {
        .signed =>
             if (int.bits <= 32) {
                return e.enif_make_int(env, @intCast(i32, value));
             } else if (int.bits <= 64) {
                return e.enif_make_int64(env, @intCast(i64, value));
             },
        .unsigned =>
            if (int.bits <= 31) {
                return e.enif_make_int(env, @intCast(i32, value));
            } else if (int.bits <= 63) {
                return e.enif_make_int64(env, @intCast(i64, value));
            } else if (int.bits == 64) {
                return e.enif_make_int64(env, @bitCast(i64, value));
            }
            // we must do something different for 64-bit unsigned integers!
    }
    // for now, panic.
    unreachable;
}