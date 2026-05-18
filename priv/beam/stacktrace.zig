const std = @import("std");
const builtin = @import("builtin");
const beam = @import("beam.zig");

// TODO: Zig 0.16.0 changed the SelfInfo API to require an Io context.
// Stacktrace symbolication is stubbed out until we can build a BEAM-based Io object.
// See: https://github.com/ziglang/zig/issues/XXXXX

pub fn to_term(stacktrace: *std.builtin.StackTrace, opts: anytype) beam.term {
    _ = stacktrace;
    // Return nil until BEAM Io integration is implemented
    return beam.make(.nil, opts);
}
