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

const builtin = @import("builtin");
const stub = @import("stub_erl_nif.zig");
const real = @cImport(@cInclude("erl_nif.h"));

pub const e = if (builtin.output_mode == .Exe) stub else real;

pub const ErlNifTerm = e.ERL_NIF_TERM;

pub usingnamespace e;