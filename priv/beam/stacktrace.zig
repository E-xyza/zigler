const std = @import("std");
const builtin = @import("builtin");
const beam = @import("beam.zig");
const DebugInfo = std.debug.DebugInfo;

var self_debug_info: ?DebugInfo = null;

fn getSelfDebugInfo() !*DebugInfo {
    if (self_debug_info) |*info| {
        return info;
    } else {
        self_debug_info = try std.debug.openSelfDebugInfo(beam.allocator);
        return &self_debug_info.?;
    }
}

fn make_empty_trace_item(env: beam.env) beam.term {
    return beam.make(env, .{
        .line_info = null,
        .symbol_name = null,
        .compile_unit_name = null,
    }, .{});
}

fn make_trace_item(env: beam.env, debug_info: *DebugInfo, address: usize) beam.term {
    const module = debug_info.getModuleForAddress(address) catch return make_empty_trace_item(env);

    const symbol_info = module.getSymbolAtAddress(beam.allocator, address) catch return make_empty_trace_item(env);
    defer symbol_info.deinit(beam.allocator);

    return beam.make(env, .{
        .line_info = symbol_info.line_info,
        .symbol_name = symbol_info.symbol_name,
        .compile_unit_name = symbol_info.compile_unit_name,
    }, .{});
}

pub fn to_term(
    env: beam.env,
    stacktrace: *std.builtin.StackTrace,
) beam.term {
    if (builtin.strip_debug_info) return beam.make(env, .nil, .{});
    const debug_info = getSelfDebugInfo() catch return beam.make(env, .nil, .{});

    var frame_index: usize = 0;
    var frames_left: usize = std.math.min(stacktrace.index, stacktrace.instruction_addresses.len);
    var stacktrace_term = beam.make_empty_list(env);

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stacktrace.instruction_addresses.len;
    }) {
        const return_address = stacktrace.instruction_addresses[frame_index];
        const new_trace_item = make_trace_item(env, debug_info, return_address - 1);
        stacktrace_term = beam.make_list_cell(env, new_trace_item, stacktrace_term);
    }
    return stacktrace_term;
}
