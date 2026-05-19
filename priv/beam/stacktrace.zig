const std = @import("std");
const builtin = @import("builtin");
const beam = @import("beam.zig");

const SelfInfo = std.debug.SelfInfo;
const Symbol = std.debug.Symbol;

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

fn getSelfInfo() *SelfInfo {
    if (self_debug_info) |*info| {
        return info;
    } else {
        self_debug_info = SelfInfo.init;
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

fn make_trace_item(debug_info: *SelfInfo, io: std.Io, address: usize, opts: anytype) beam.term {
    const allocator = getDebugInfoAllocator();

    var symbols: std.ArrayList(Symbol) = .empty;
    defer symbols.deinit(allocator);

    debug_info.getSymbols(io, allocator, allocator, address, false, &symbols) catch {
        return make_empty_trace_item(opts);
    };

    if (symbols.items.len == 0) {
        return make_empty_trace_item(opts);
    }

    const symbol = symbols.items[0];

    return beam.make(.{
        .source_location = symbol.source_location,
        .symbol_name = symbol.name,
        .compile_unit_name = symbol.compile_unit_name,
    }, opts);
}

pub fn to_term(stacktrace: *std.builtin.StackTrace, opts: anytype) beam.term {
    if (builtin.strip_debug_info) return beam.make(.nil, opts);

    const debug_info = getSelfInfo();
    const io = beam.context.io;

    var frame_index: usize = 0;
    var frames_left: usize = @min(stacktrace.index, stacktrace.instruction_addresses.len);
    var stacktrace_term = beam.make_empty_list(opts);

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stacktrace.instruction_addresses.len;
    }) {
        const return_address = stacktrace.instruction_addresses[frame_index];
        const new_trace_item = make_trace_item(debug_info, io, return_address -| 1, opts);
        stacktrace_term = beam.make_list_cell(new_trace_item, stacktrace_term, opts);
    }
    return stacktrace_term;
}
