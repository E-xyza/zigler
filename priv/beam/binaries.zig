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

pub fn binary_to_term(env: beam.env, binary: []u8, opts: anytype) !beam.term {
    const safe = if (@hasField(@TypeOf(opts), "safe")) opts.safe else false;
    const fn_opt = if (safe) e.ERL_NIF_BIN2TERM_SAFE else 0;
    var result: beam.term = undefined;
    if (e.enif_binary_to_term(env, binary.ptr, binary.len, &result.v, fn_opt) == 0) return error.OutOfMemory;
    return result;
}

pub fn release_binary(binary: *e.ErlNifBinary) void {
    e.enif_release_binary(binary);
}
