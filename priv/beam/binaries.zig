const e = @import("erl_nif.zig");
const beam = @import("beam.zig");

pub fn binary_to_slice(binary: e.ErlNifBinary) []u8 {
    return binary.data[0..binary.size];
}

pub fn term_to_binary(env: beam.env, term: beam.term) !e.ErlNifBinary {
    var binary: e.ErlNifBinary = undefined;
    if (e.enif_term_to_binary(env, term.v, &binary) == 0) return error.OutOfMemory;
    return binary;
}

pub fn binary_to_term(env: beam.env, binary: []u8) !beam.term {
    var result: beam.term = undefined;
    if (e.enif_binary_to_term(env, binary.ptr, binary.len, &result.v, 0) == 0) return error.OutOfMemory;
    return result;
}

pub fn release_binary(binary: *e.ErlNifBinary) void {
    e.enif_release_binary(binary);
}
