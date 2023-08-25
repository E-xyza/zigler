const beam = @import("beam.zig");
const e = @import("erl_nif");
const threads = @import("threads.zig");

const PidError = error{ NotProcessBound, NotDelivered };

pub fn self(env: beam.env) PidError!beam.pid {
    var pid: beam.pid = undefined;
    switch (beam.context) {
        .threaded => {
            return threads.self_pid();
        },
        .callback => {
            return error.NotProcessBound;
        },
        else => {
            if (e.enif_self(env, &pid)) |_| {
                return pid;
            } else {
                return error.NotProcessBound;
            }
        },
    }
}

pub fn send(env: beam.env, dest: beam.pid, content: anytype) PidError!beam.term {
    beam.ignore_when_sema();

    const term = beam.make(env, content, .{});

    // enif_send is not const-correct so we have to assign a variable to the static
    // pid variable

    var pid = dest;
    // disable this in sema because pid pointers are not supported

    switch (beam.context) {
        .synchronous, .callback, .dirty => {
            if (e.enif_send(env, &pid, null, term.v) == 0) return error.NotDelivered;
        },
        .threaded, .yielding => {
            if (e.enif_send(null, &pid, env, term.v) == 0) return error.NotDelivered;
        },
    }
    return term;
}
