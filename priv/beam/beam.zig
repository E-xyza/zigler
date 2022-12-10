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

const is_sema = switch (builtin.output_mode) {
  .Exe => true,
  .Lib => false,
  else => unreachable
};

// semantic analysis
pub const sema = if (is_sema) @import("sema.zig") else void;

// loading boilerplate
pub const loader = @import("loader.zig");

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
pub const term = if (is_sema) struct {
  v: e.ErlNifTerm,
  pub fn term_type(_: *const @This(), _: env) TermType { return .atom; }
} else packed struct {
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
pub const make_cpointer = make_.make_cpointer;
pub const make_binary = make_.make_binary;
pub const make_empty_list = make_.make_empty_list;
pub const make_error_atom = make_.make_error_atom;
pub const make_error_pair = make_.make_error_pair;

///////////////////////////////////////////////////////////////////////////////
// options

const zigler_options = @import("zigler_options");

pub const options = struct{
    pub const use_gpa = if (@hasDecl(zigler_options, "use_gpa")) zigler_options.use_gpa else false;
};

///////////////////////////////////////////////////////////////////////////////
// allocators

const allocator_ = @import("allocator.zig");

pub const make_general_purpose_allocator_instance = allocator_.make_general_purpose_allocator_instance;
pub var general_purpose_allocator_instance = make_general_purpose_allocator_instance();
pub const general_purpose_allocator = general_purpose_allocator_instance.allocator();

///
/// wraps `erl_nif_alloc` and `erl_nif_free` into the zig allocator interface.
/// does a very simple implementation of this in the default case.
/// you may also set the `use_gpa` option, which will make the default beam allocator
/// the BEAM allocator wrapped in zig stdlib's `general_purpose_allocator`.
/// This will provide you with thread-safety, double-free-safety, and the ability to
/// check that there are no memory leaks.
///
pub threadlocal var allocator = if (options.use_gpa) general_purpose_allocator else allocator_.raw_beam_allocator;

///////////////////////////////////////////////////////////////////////////////
// exception

pub fn raise_exception(env_: env, reason: anytype) term {
    return term{.v = e.enif_raise_exception(env_, make(env_, reason, .{}).v)};
}

pub fn raise_elixir_exception(env_: env, comptime module: []const u8, data: anytype) term {
    if (@typeInfo(@TypeOf(data)) != .Struct) {
        @compileError("elixir exceptions must be structs");
    }

    const name = comptime name: { break :name "Elixir." ++ module; };
    var exception: e.ErlNifTerm = undefined;
    const initial = make(env_, data, .{});

    _ = e.enif_make_map_put(env_, initial.v, make_into_atom(env_, "__struct__").v, make_into_atom(env_, name).v, &exception);
    _ = e.enif_make_map_put(env_, exception, make_into_atom(env_, "__exception__").v, make(env_, true, .{}).v, &exception);

    return raise_exception(env_, term{.v = exception});
}