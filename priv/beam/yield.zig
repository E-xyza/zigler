const beam = @import("beam.zig");
const threads = @import("threads.zig");
const e = @import("erl_nif");

pub fn yield() !void {
    switch (beam.context.mode) {
        .threaded => try threads.yield(),
        .yielding => @panic("not completed yet"),
        else => {
            if (e.enif_is_current_process_alive(beam.context.env) == 0) {
                return error.processterminated;
            }
        },
    }
}
