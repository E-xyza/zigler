const std = @import("std");
const builtin = @import("builtin");
const beam = @import("beam.zig");
const options = @import("options.zig");

const DebugInfo = std.debug.DebugInfo;

var self_debug_info: ?DebugInfo = null;


fn getSelfDebugInfo(opts: anytype) !*DebugInfo {
    if (self_debug_info) |*info| {
        return info;
    } else {
        self_debug_info = try std.debug.openSelfDebugInfo(options.allocator(opts));
        return &self_debug_info.?;
    }
}

fn make_empty_trace_item(opts: anytype) beam.term {
    return beam.make(.{
        .line_info = null,
        .symbol_name = null,
        .compile_unit_name = null,
    }, opts);
}

fn make_trace_item(debug_info: *DebugInfo, address: usize, opts: anytype) beam.term {
    const module = debug_info.getModuleForAddress(address) catch return make_empty_trace_item(opts);
    const symbol_info = module.getSymbolAtAddress(beam.allocator, address) catch return make_empty_trace_item(opts);

    defer symbol_info.deinit(options.allocator(opts));

    return beam.make(.{
        .line_info = symbol_info.line_info,
        .symbol_name = symbol_info.symbol_name,
        .compile_unit_name = symbol_info.compile_unit_name,
    }, opts);
}

pub fn to_term(
    stacktrace: *std.builtin.StackTrace,
    opts: anytype
) beam.term {
    if (builtin.strip_debug_info) return beam.make(.nil, opts);
    const debug_info = getSelfDebugInfo(opts) catch return beam.make(.nil, opts);

    var frame_index: usize = 0;
    var frames_left: usize = @min(stacktrace.index, stacktrace.instruction_addresses.len);
    var stacktrace_term = beam.make_empty_list(opts);

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stacktrace.instruction_addresses.len;
    }) {
        const return_address = stacktrace.instruction_addresses[frame_index];
        const new_trace_item = make_trace_item(debug_info, return_address - 1, opts);
        stacktrace_term = beam.make_list_cell(new_trace_item, stacktrace_term, opts);
    }
    return stacktrace_term;
}
