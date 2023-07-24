const beam = @import("beam.zig");
const threads = @import("threads.zig");
const e = @import("erl_nif.zig");

pub fn yield(env: beam.env) !void {
    switch (beam.context) {
        .threaded => try threads.yield(),
        .yielding => @panic("not completed yet"),
        else => {
            if (e.enif_is_current_process_alive(env) == 0) {
                return error.processterminated;
            }}
    }
}
