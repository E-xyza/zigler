const beam = @import("beam.zig");
const threads = @import("threads.zig");

pub fn yield() !void {
    switch (beam.context) {
        .threaded => try threads.yield(),
        .yielding => @panic("not completed yet"),
        else => {}  // no-op on other contexts.
    }
}