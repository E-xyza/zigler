//! This struct contains adapters designed to facilitate interfacing the
//! BEAM's c-style helpers for NIFs with a more idiomatic Zig-style of
//! programming, for example, the use of slices instead of null-terminated
//! arrays as strings.
//!
//! This struct derives from `zig/beam/beam.zig`, and you may import it into
//! your module's zig code by calling:
//!
//! ```
//! const beam = @import("beam.zig")
//! ```
//!
//! This is done automatically for you inside your `~Z` forms, so do NOT
//! use this import statement with inline Zig.
//!
//! ## Features
//!
//! ### The BEAM Allocator
//!
//! Wraps `e.enif_alloc` and `e.enif_free` functions into a compliant Zig
//! allocator struct.  You should thus be able to supply Zig standard library
//! functions which require an allocator a struct that is compliant with its
//! requirements.
//!
//! This is, in particular, useful for slice generation.
//!
//! #### Example (slice generation)
//!
//! ```
//! beam = @import("beam.zig");
//!
//! fn make_a_slice_of_floats() ![]f32 {
//!   return beam.allocator.alloc(f32, 100);
//! }
//! ```
//!
//! Because Zig features *composable allocators*, you can very easily implement
//! custom allocators on top of the existing BEAM allocator.
//!
//! ### Getters
//!
//! Erlang's NIF interface provides a comprehensive set of methods to retrieve
//! data out of BEAM terms.  However, this set of methods presents an error
//! handling scheme that is designed for C and inconsistent with the idiomatic
//! scheme used for Zig best practices.
//!
//! A series of get functions is provided, implementing these methods in
//! accordance to best practices.  These include `get/3`, which is the generic
//! method for getting scalar values, `get_X`, which are typed methods for
//! retrieving scalar values, and `get_slice_of/3`, which is the generic method
//! for retrieving a Zig slice from a BEAM list.
//!
//! Naturally, for all of these functions, you will have to provide the BEAM
//! environment value.
//!
//! #### Examples
//!
//! ```
//! const beam = @import("beam.zig");
//!
//! fn double_value(env: beam.env, value: beam.term) !f64 {
//!   return (try beam.get_f64(env, value)) * 2;
//! }
//!
//! fn sum_float_list(env: beam.env, list: beam.term) !f64 {
//!   zig_list: []f64 = try beam.get_slice_of(f64, env, list);
//!   defer beam.allocator.free(zig_list);  // don't forget to clean up!
//!
//!   result: f64 = 0;
//!   for (list) |item| { result += item; }
//!   return result;
//! }
//! ```
//!
//! ### Makers
//!
//! A series of "make" functions is provided which allow for easy export of
//! Zig values back to the BEAM.  Typically, these functions are used in the
//! automatic type marshalling performed by Zigler, however, you may want to
//! be able to use them yourself to assemble BEAM datatypes not directly
//! supported by Zig.  For example, a custom tuple value.
//!
//! #### Example
//!
//! ```
//! const beam = @import("beam.zig");
//!
//! const ok_slice="ok"[0..];
//! fn to_ok_tuple(env: beam.env, value: i64) !beam.term {
//!   var tuple_slice: []term = try beam.allocator.alloc(beam.term, 2);
//!   defer beam.allocator.free(tuple_slice);
//!
//!   tuple_slice[0] = beam.make_atom(env, ok_slice);
//!   tuple_slice[1] = beam.make_i64(env, value);
//!
//!   return beam.make_tuple(env, tuple_slice);
//! }
//!
//! ```

const e = @import("erl_nif.zig");
const std = @import("std");
const builtin = @import("builtin");
const BeamMutex = @import("mutex.zig").BeamMutex;

// semantic analysis
pub const sema = if (builtin.output_mode == .Exe) @import("sema.zig") else void;

// loading boilerplate
pub const loader = if (builtin.output_mode == .Lib) @import("loader.zig") else void;

/// syntactic sugar for the BEAM environment.  Note that the `env` type
/// encapsulates the pointer, since you will almost always be passing this
/// pointer to an opaque struct around without accessing it.
pub const env = ?*e.ErlNifEnv;

const TermType = enum (u64) {
  atom = e.ERL_NIF_TERM_TYPE_ATOM,
  bitstring = e.ERL_NIF_TERM_TYPE_BITSTRING,
  float = e.ERL_NIF_TERM_TYPE_FLOAT,
  fun = e.ERL_NIF_TERM_TYPE_FUN,
  integer = e.ERL_NIF_TERM_TYPE_INTEGER,
  list = e.ERL_NIF_TERM_TYPE_LIST,
  map = e.ERL_NIF_TERM_TYPE_MAP,
  pid = e.ERL_NIF_TERM_TYPE_PID,
  port = e.ERL_NIF_TERM_TYPE_PORT,
  ref = e.ERL_NIF_TERM_TYPE_REFERENCE,
  tuple = e.ERL_NIF_TERM_TYPE_TUPLE
};

/// wrapped term struct.  This lets us typecheck terms on the way in and out.
/// many things will still if you use the "original" e.ErlNifTerm, but some things
/// will require you to use `beam.term` instead.
pub const term = struct {
    v: e.ErlNifTerm,

    /// equivalent of e.enif_term_type
    pub fn term_type(this: *const @This(), environment: env) TermType {
        return @intToEnum(TermType, e.enif_term_type(environment, this.v));
    }
};

///////////////////////////////////////////////////////////////////////////////
// generics

const get_ = @import("get.zig");
const make_ = @import("make.zig");

pub const get = get_.get;
pub const make = make_.make;
pub const make_into_atom = make_.make_into_atom;

///////////////////////////////////////////////////////////////////////////////
// allocators

const allocator_ = @import("allocator.zig");

pub const allocator = allocator_.allocator;

