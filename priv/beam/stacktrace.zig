const std = @import("std");
const builtin = @import("builtin");
const beam = @import("beam.zig");
const options = @import("options.zig");

const SelfInfo = std.debug.SelfInfo;

var self_debug_info: ?SelfInfo = null;

var debug_info_allocator: ?std.mem.Allocator = null;
var debug_info_arena_allocator: std.heap.ArenaAllocator = undefined;
fn getDebugInfoAllocator() std.mem.Allocator {
    if (debug_info_allocator) |a| return a;

    debug_info_arena_allocator = std.heap.ArenaAllocator.init(beam.allocator);
    const allocator = debug_info_arena_allocator.allocator();
    debug_info_allocator = allocator;
    return allocator;
}

fn getSelfInfo() !*SelfInfo {
    if (self_debug_info) |*info| {
        return info;
    } else {
        self_debug_info = try SelfInfo.open(getDebugInfoAllocator());
        return &self_debug_info.?;
    }
}

fn make_empty_trace_item(opts: anytype) beam.term {
    return beam.make(.{
        .source_location = null,
        .symbol_name = null,
        .compile_unit_name = null,
    }, opts);
}

fn make_trace_item(debug_info: *SelfInfo, address: usize, opts: anytype) beam.term {
    const module = debug_info.getModuleForAddress(address) catch return make_empty_trace_item(opts);
    //defer module.deinit(debug_info_allocator.?);

    const symbol_info = module.getSymbolAtAddress(std.heap.c_allocator, address) catch return make_empty_trace_item(opts);

    return beam.make(.{
        .source_location = symbol_info.source_location,
        .symbol_name = symbol_info.name,
        .compile_unit_name = symbol_info.compile_unit_name,
    }, opts);
}

pub fn to_term(stacktrace: *std.builtin.StackTrace, opts: anytype) beam.term {
    if (builtin.strip_debug_info) return beam.make(.nil, opts);

    const debug_info = getSelfInfo() catch return beam.make(.nil, opts);
    // NOTE: we should never deinit the debug_info, as it doesn't change over the execution of
    // the process.  It will be stored as a "global object".
    
    var frame_index: usize = 0;
    var frames_left: usize = @min(stacktrace.index, stacktrace.instruction_addresses.len);
    var stacktrace_term = beam.make_empty_list(opts);

    stacktrace_term = stacktrace_term;

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stacktrace.instruction_addresses.len;
    }) {
        const return_address = stacktrace.instruction_addresses[frame_index];
        const new_trace_item = make_trace_item(debug_info, return_address -| 1, opts);
        stacktrace_term = beam.make_list_cell(new_trace_item, stacktrace_term, opts);
    }
    return stacktrace_term;
}
