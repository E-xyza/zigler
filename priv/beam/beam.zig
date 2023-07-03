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

pub const is_sema = @import("zigler_options").sema;

pub inline fn ignore_when_sema() void {
    if (is_sema) unreachable;
}

// loading boilerplate
pub const loader = @import("loader.zig");

/// syntactic sugar for the BEAM environment.  Note that the `env` type
/// encapsulates the pointer, since you will almost always be passing this
/// pointer to an opaque struct around without accessing it.
pub const env = ?*e.ErlNifEnv;

pub const pid = e.ErlNifPid;
pub const port = e.ErlNifPort;

const TermType = enum(u64) { atom = e.ERL_NIF_TERM_TYPE_ATOM, bitstring = e.ERL_NIF_TERM_TYPE_BITSTRING, float = e.ERL_NIF_TERM_TYPE_FLOAT, fun = e.ERL_NIF_TERM_TYPE_FUN, integer = e.ERL_NIF_TERM_TYPE_INTEGER, list = e.ERL_NIF_TERM_TYPE_LIST, map = e.ERL_NIF_TERM_TYPE_MAP, pid = e.ERL_NIF_TERM_TYPE_PID, port = e.ERL_NIF_TERM_TYPE_PORT, ref = e.ERL_NIF_TERM_TYPE_REFERENCE, tuple = e.ERL_NIF_TERM_TYPE_TUPLE };

/// wrapped term struct.  This lets us typecheck terms on the way in and out.
/// many things will still if you use the "original" e.ErlNifTerm, but some things
/// will require you to use `beam.term` instead.
pub const term = if (is_sema) struct {
    v: e.ErlNifTerm,
    pub fn term_type(_: *const @This(), _: env) TermType {
        return .atom;
    }
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
const cleanup_ = @import("cleanup.zig");
const processes = @import("processes.zig");
const stacktrace = @import("stacktrace.zig");
const payload = @import("payload.zig");

/// get function.
pub const get = get_.get;
pub const make = make_.make;
pub const cleanup = cleanup_.cleanup;
pub const self = processes.self;
pub const send = processes.send;

// special makers
pub const make_pid = make_.make_pid;
pub const make_into_atom = make_.make_into_atom;
pub const make_cpointer = make_.make_cpointer;
pub const make_binary = make_.make_binary;
pub const make_empty_list = make_.make_empty_list;
pub const make_list_cell = make_.make_list_cell;
pub const make_error_atom = make_.make_error_atom;
pub const make_error_pair = make_.make_error_pair;
pub const make_ref = make_.make_ref;
pub const make_stacktrace = stacktrace.to_term;

pub const Payload = payload.Payload;

//////////////////////////////////////////////////////////////////////////////
// binaries

const binaries = @import("binaries.zig");
pub const binary_to_slice = binaries.binary_to_slice;
pub const term_to_binary = binaries.term_to_binary;
pub const binary_to_term = binaries.binary_to_term;
pub const release_binary = binaries.release_binary;

//////////////////////////////////////////////////////////////////////////////
// special functions

const ExecutionContext = enum { process_bound, threaded, dirty, yielding, callback };
pub threadlocal var context: ExecutionContext = .process_bound;

// these atoms are used to conform to Elixir's Compare interface
// see: https://hexdocs.pm/elixir/1.13/Enum.html#sort/2-sorting-structs
pub const Compared = enum { lt, eq, gt };

/// compares two terms.
pub fn compare(lhs: term, rhs: term) Compared {
    const compared = e.enif_compare(lhs.v, rhs.v);

    if (compared == 0) return .eq;
    if (compared < 0) return .lt;
    if (compared > 0) return .gt;
    unreachable;
}

///////////////////////////////////////////////////////////////////////////////
// options

const zigler_options = @import("zigler_options");

///////////////////////////////////////////////////////////////////////////////
// allocators

const allocator_ = @import("allocator.zig");

pub const general_purpose_allocator = allocator_.general_purpose_allocator;
pub const raw_beam_allocator = allocator_.raw_beam_allocator;

/// wraps `erl_nif_alloc` and `erl_nif_free` into the zig allocator interface.
/// does a very simple implementation of this in the default case.
/// you may also set the `use_gpa` option, which will make the default beam allocator
/// the BEAM allocator wrapped in zig stdlib's `general_purpose_allocator`.
/// This will provide you with thread-safety, double-free-safety, and the ability to
/// check that there are no memory leaks.

pub threadlocal var allocator = allocator_.raw_beam_allocator;

///////////////////////////////////////////////////////////////////////////////
// resources

pub const resource = @import("resource.zig");
pub const Resource = resource.Resource;

pub const event = e.ErlNifEvent;
pub const monitor = e.ErlNifMonitor;

///////////////////////////////////////////////////////////////////////////////
// env management

pub const alloc_env = e.enif_alloc_env;
pub const free_env = e.enif_free_env;

pub fn copy(env_: env, term_: term) term {
    return .{ .v = e.enif_make_copy(env_, term_.v) };
}

///////////////////////////////////////////////////////////////////////////////
// threads

pub const tid = e.ErlNifTid;

const threads = @import("threads.zig");
pub const Thread = threads.Thread;
pub const ThreadedCallbacks = threads.Callbacks;

///////////////////////////////////////////////////////////////////////////////
// yields

const yield_ = @import("yield.zig");
pub const yield = yield_.yield;
//pub const YieldingFrame = yield_.Frame;
//pub const YieldingCallbacks = yield_.Callbacks;

///////////////////////////////////////////////////////////////////////////////
// exception

pub fn raise_exception(env_: env, reason: anytype) term {
    return term{ .v = e.enif_raise_exception(env_, make(env_, reason, .{}).v) };
}

pub fn raise_elixir_exception(env_: env, comptime module: []const u8, data: anytype) term {
    if (@typeInfo(@TypeOf(data)) != .Struct) {
        @compileError("elixir exceptions must be structs");
    }

    const name = comptime name: {
        break :name "Elixir." ++ module;
    };
    var exception: e.ErlNifTerm = undefined;
    const initial = make(env_, data, .{});

    _ = e.enif_make_map_put(env_, initial.v, make_into_atom(env_, "__struct__").v, make_into_atom(env_, name).v, &exception);
    _ = e.enif_make_map_put(env_, exception, make_into_atom(env_, "__exception__").v, make(env_, true, .{}).v, &exception);

    return raise_exception(env_, term{ .v = exception });
}

///////////////////////////////////////////////////////////////////////////////
// ETC

// wrappedresult: for yielding and threaded nifs we have to do something a bit
// different to wrap a zig error across the beam boundary.  This common utility
// type is used for that.

const WrappedResultTag = enum { ok, error_return_trace };

/// <!-- ignore -->
pub fn WrappedResult(comptime FunctionType: type) type {
    const NaiveReturnType = @typeInfo(FunctionType).Fn.return_type.?;
    return switch (@typeInfo(NaiveReturnType)) {
        .ErrorUnion => |eu| union(WrappedResultTag) {
            ok: eu.payload,
            error_return_trace: term,
        },
        else => NaiveReturnType,
    };
}
