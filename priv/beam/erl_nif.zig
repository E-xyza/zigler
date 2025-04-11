//! Provides erlang's NIF convenience functions to your Zig code.
//!
//! This struct derives from `zig/beam/erl_nif.zig`.  Usually brought in as
//! `const e = @import("erl_nif").c;`, leading to the aliased shortcut `e`.
//!
//! for example, to call the enif_alloc function, you would use the following
//! code:
//!
//! ```
//! const e = @import("erl_nif").c;
//!
//! pub fn give_me_ten_bytes() ?*u8 {
//!   return e.enif_alloc(10);
//! }
//! ```
//!
//! refer to the [erlang documentation](https://erlang.org/doc/man/erl_nif.html)
//! for available functions

// note: currently, zig is incapable of substituting a macro variable for a
// C struct field.  Until that is fixed
const builtin = @import("builtin");

const e = if (builtin.os.tag == .windows) @cImport(@cInclude("erl_nif_win.h")) else @cImport(@cInclude("erl_nif.h"));

pub const ErlNifTerm = e.ERL_NIF_TERM;

pub const is_sema = false;

pub usingnamespace e;
