const e = @import("erl_nif");
const beam = @import("beam.zig");
const options = @import("options.zig");

pub fn binary_to_slice(binary: e.ErlNifBinary) []u8 {
    return binary.data[0..binary.size];
}

pub fn term_to_binary(term: beam.term, opts: anytype) !e.ErlNifBinary {
    var binary: e.ErlNifBinary = undefined;
    if (e.enif_term_to_binary(options.env(opts), term.v, &binary) == 0) return error.OutOfMemory;
    return binary;
}

pub fn binary_to_term(binary: []const u8, opts: anytype) !beam.term {
    var result: beam.term = undefined;
    if (e.enif_binary_to_term(options.env(opts), binary.ptr, binary.len, &result.v, 0) == 0) return error.OutOfMemory;
    return result;
}

pub fn release_binary(binary: *e.ErlNifBinary) void {
    e.enif_release_binary(binary);
}
