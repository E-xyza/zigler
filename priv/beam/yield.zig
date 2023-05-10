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

fn is_thread_joining() bool {
    return @atomicLoad(bool, threads.thread_joining, .Monotonic);
}

fn yield_threaded() !void {
    if (is_thread_joining()) {
        return error.process_terminated;
    }
}