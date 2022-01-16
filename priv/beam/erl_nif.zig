//! Provides erlang's NIF convenience functions to your Zig code.
//!
//! This struct derives from `zig/beam/erl_nif.zig`.  Usually brought in as
//! `const e = @import("erl_nif.zig").c;`, leading to the aliased shortcut `e`.
//!
//! for example, to call the enif_alloc function, you would use the following
//! code:
//!
//! ```
//! const e = @import("erl_nif.zig").c;
//!
//! pub fn give_me_ten_bytes() ?*u8 {
//!   return e.enif_alloc(10);
//! }
//! ```
//!
//! refer to the [erlang documentation](https://erlang.org/doc/man/erl_nif.html)
//! for available functions

const _erl_nif = @cImport({
  @cInclude("erl_nif.h");
});

pub usingnamespace _erl_nif;

pub const ErlNifTerm = _erl_nif.ERL_NIF_TERM;
