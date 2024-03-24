const beam = @import("beam.zig");
const e = @import("erl_nif");
const threads = @import("threads.zig");

const PidError = error{ NotProcessBound, NotDelivered };

pub fn self(opts: anytype) PidError!beam.pid {
    var pid: beam.pid = undefined;
    switch (beam.context.mode) {
        .threaded => {
            return threads.self_pid();
        },
        .callback => {
            return error.NotProcessBound;
        },
        else => {
            if (e.enif_self(env(opts), &pid)) |_| {
                return pid;
            } else {
                return error.NotProcessBound;
            }
        },
    }
}

pub fn send(dest: beam.pid, content: anytype, opts: anytype) PidError!beam.term {
    beam.ignore_when_sema();

    const term = beam.make(content, opts);

    // enif_send is not const-correct so we have to assign a variable to the static
    // pid variable

    var pid = dest;
    // disable this in sema because pid pointers are not supported

    switch (beam.context.mode) {
        .synchronous, .callback, .dirty => {
            if (e.enif_send(env(opts), &pid, null, term.v) == 0) return error.NotDelivered;
        },
        .threaded, .yielding => {
            if (e.enif_send(null, &pid, env(opts), term.v) == 0) return error.NotDelivered;
        },
    }
    return term;
}

inline fn env(opts: anytype) beam.env {
    const T = @TypeOf(opts);
    if (@hasField(T, "env")) {
        return opts.env;
    }
    return beam.context.env;
}