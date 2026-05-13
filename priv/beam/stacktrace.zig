const std = @import("std");
const beam = @import("beam.zig");

// Zig 0.16 stub.
//
// `std.debug.SelfInfo` was substantially restructured in Zig 0.16 —
// constructed via a zero-value `init` const (not an `open(allocator)`
// function), all methods now take an `io: Io` parameter, and the
// per-OS implementations (MachO/Elf/Pdb) moved into a `SelfInfo/`
// subdirectory.
//
// Properly porting this file means plumbing an `Io` instance through
// every call site and re-fitting the `getModuleForAddress` +
// `getSymbolAtAddress` flow to the new signatures.
//
// For the initial 0.16 port we degrade gracefully: NIF errors still
// get reported to BEAM as normal, just without the per-frame source
// location list. The cost is that crashed-NIF error messages on the
// Elixir side lose the file:line annotations they had under 0.15.
// Marked TODO so the proper port lands as a focused follow-up after
// the rest of the 0.16 port is verified end-to-end.

pub fn to_term(stacktrace: *std.builtin.StackTrace, opts: anytype) beam.term {
    _ = stacktrace;
    return beam.make(.nil, opts);
}
