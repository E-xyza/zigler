const beam = @import("beam.zig");
const threads = @import("threads.zig");

const YieldError = error{process_terminated};

pub fn yield(_: anytype) !void {
    switch (beam.context) {
        .threaded => try yield_threaded(),
        .yielding => @panic("not completed yet"),
        else => {}  // no-op on other contexts.
    }
}

fn yield_threaded() !void {
    switch (threads.thread_state.?.*) {
        .done => return error.process_terminated,
        .launched => return,
        .prepped => @panic("yield should not be called when thread is only prepped"),
        .joined => @panic("yield should not be called when thread has been joined"),
    }
}