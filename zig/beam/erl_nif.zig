/// this struct derives from `zig/beam/erl_nif.zig` 
///
/// refer to the [erlang documentation](https://erlang.org/doc/man/erl_nif.html)
/// for available functions
///
/// WARNING: currently, `erl_nif.h` is manually translated from the erlang 
/// header to be more digestible, and some functions available in the BEAM may 
/// not have been implemented yet, and windows support is currently not
/// possible.


pub const c = @cImport({
  @cInclude("<%= erl_nif_zig_h %>");
});