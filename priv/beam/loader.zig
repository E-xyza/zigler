///////////////////////////////////////////////////////////////////////////////
// NIF LOADING Boilerplate functions.

const beam = @import("beam.zig");
const e = @import("erl_nif.zig");

pub export fn blank_load(
  _: beam.env,
  _: [*c]?*anyopaque,
  _: e.ErlNifTerm) c_int {
  return 0;
}