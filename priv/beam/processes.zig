const beam = @import("beam.zig");
const e = @import("erl_nif");
const options = @import("options.zig");
const threads = @import("threads.zig");
const std = @import("std");

const PidError = error{ NotProcessBound, Dead, NotDelivered };

pub fn self(opts: anytype) PidError!beam.pid {
    var pid: beam.pid = undefined;
    switch (beam.context.mode) {
        .threaded => return threads.self_pid(),
        .callback, .independent => return error.NotProcessBound,
        .dirty_yield => return error.Dead,
        .yielding => @panic("not supported yet"),
        else => {
            if (e.enif_self(options.env(opts), &pid)) |_| {
                return pid;
            } else {
                return error.NotProcessBound;
            }
        },
    }
}

fn SendReturnType(Opts: type) type {
    if (@typeInfo(Opts) != .@"struct") @compileError("opts must be a tuple");
    inline for (@typeInfo(Opts).@"struct".fields) |field| {
        if (std.mem.eql(u8, field.name, "persist")) {
            return field.type;
        }
    }
    return void;
}

pub fn send(dest: beam.pid, content: anytype, opts: anytype) PidError!SendReturnType(@TypeOf(opts)) {
    beam.ignore_when_sema();

    const term = beam.make(content, opts);

    var pid = dest;

    switch (beam.context.mode) {
        .synchronous, .callback, .dirty => {
            if (e.enif_send(options.env(opts), @constCast(&pid), null, term.v) == 0) return error.NotDelivered;
            if (@hasField(@TypeOf(opts), "persist")) {
                if (beam.SendReturnType(opts) == void) return;
                return opts.persist;
            }
        },
        .threaded, .yielding, .dirty_yield, .independent => {
            if (e.enif_send(null, @constCast(&pid), options.env(opts), term.v) == 0) return error.NotDelivered;

            if (options.should_clear(opts)) {
                if (@hasField(@TypeOf(opts), "persist")) {
                    return beam.clear_env(options.env(opts), .{ .persist = options.persist });
                }
            }
        },
    }
}
