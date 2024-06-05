///////////////////////////////////////////////////////////////////////////////
// NIF LOADING Boilerplate functions.

const beam = @import("beam.zig");
const e = @import("erl_nif");

pub export fn blank_load(_: beam.env, _: ?*?*anyopaque, _: e.ErlNifTerm) c_int {
    return 0;
}

pub export fn blank_upgrade(_: beam.env, _: ?*?*anyopaque, _: ?*?*anyopaque, _: e.ErlNifTerm) c_int {
    return 0;
}

pub export fn blank_unload(_: beam.env, _: ?*anyopaque) void {}
