const beam = @import("beam.zig");
const e = @import("erl_nif");
const options = @import("options.zig");
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
            if (e.enif_self(options.env(opts), &pid)) |_| {
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

    var pid = dest;

    switch (beam.context.mode) {
        .synchronous, .callback, .dirty => {
            if (e.enif_send(options.env(opts), @constCast(&pid), null, term.v) == 0) return error.NotDelivered;
        },
        .threaded, .yielding, .dirty_yield => {
            defer {
                if (options.should_clear(opts)) {
                    beam.clear_env(options.env(opts));
                }
            }

            if (e.enif_send(null, @constCast(&pid), options.env(opts), term.v) == 0) return error.NotDelivered;
        },
    }
    return term;
}
