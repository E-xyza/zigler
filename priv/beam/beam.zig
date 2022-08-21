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
const BeamMutex = @import("beam_mutex.zig").BeamMutex;

// semantic analysis
pub const sema = @import("sema.zig");

///////////////////////////////////////////////////////////////////////////////
// BEAM allocator definitions
///////////////////////////////////////////////////////////////////////////////

const Allocator = std.mem.Allocator;

// basic allocator

/// !value
/// provides a default BEAM allocator.  This is an implementation of the Zig
/// allocator interface.  Use `beam.allocator.alloc` everywhere to safely
/// allocate slices efficiently, and use `beam.allocator.free` to release that
/// memory.  For single item allocation, use `beam.allocator.create` and
/// `beam.allocator.destroy` to release the memory.
///
/// Note this does not make the allocated memory *garbage collected* by the
/// BEAM.
///
/// All memory will be tracked by the beam.  All allocations happen with 8-byte
/// alignment, as described in `erl_nif.h`.  This is sufficient to create
/// correctly aligned `beam.terms`, and for most purposes.
/// For data that require greater alignment, use `beam.large_allocator`.
///
/// ### Example
///
/// The following code will return ten bytes of new memory.
///
/// ```zig
/// const beam = @import("beam.zig");
///
/// fn give_me_ten_bytes() ![]u8 {
///   return beam.allocator.alloc(u8, 10);
/// }
/// ```
///
/// currently does not release memory that is resized.  For this behaviour, use
/// use `beam.general_purpose_allocator`.
///
/// not threadsafe.  for a threadsafe allocator, use `beam.general_purpose_allocator`
pub const allocator = raw_beam_allocator;

pub const MAX_ALIGN = 8;

const raw_beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &raw_beam_allocator_vtable,
};
const raw_beam_allocator_vtable = Allocator.VTable{
    .alloc = raw_beam_alloc,
    .resize = raw_beam_resize,
    .free = raw_beam_free,
};

fn raw_beam_alloc(
    _: *anyopaque,
    len: usize,
    ptr_align: u29,
    _: u29,
    _: usize,
) Allocator.Error![]u8 {
  if (ptr_align > MAX_ALIGN) { return error.OutOfMemory; }
  const ptr = e.enif_alloc(len) orelse return error.OutOfMemory;
  return @ptrCast([*]u8, ptr)[0..len];
}

fn raw_beam_resize(
    _: *anyopaque,
    buf: []u8,
    _: u29,
    new_len: usize,
    _: u29,
    _: usize,
) ?usize {
  if (new_len == 0) {
    e.enif_free(buf.ptr);
    return 0;
  }
  if (new_len <= buf.len) {
    return new_len;
  }
  // Is this the right thing to do???
  return null;
}

fn raw_beam_free(
    _: *anyopaque,
    buf: []u8,
    _: u29,
    _: usize,
) void {
    e.enif_free(buf.ptr);
}

/// !value
/// provides a BEAM allocator that can perform allocations with greater
/// alignment than the machine word.  Note that this comes at the cost
/// of some memory to store important metadata.
///
/// currently does not release memory that is resized.  For this behaviour
/// use `beam.general_purpose_allocator`.
///
/// not threadsafe.  for a threadsafe allocator, use `beam.general_purpose_allocator`
pub const large_allocator = large_beam_allocator;

const large_beam_allocator = Allocator{
    .ptr = undefined,
    .vtable = &large_beam_allocator_vtable,
};
const large_beam_allocator_vtable = Allocator.VTable{
    .alloc = large_beam_alloc,
    .resize = large_beam_resize,
    .free = Allocator.NoOpFree(anyopaque).noOpFree,
};

fn large_beam_alloc(_: *anyopaque, len: usize, alignment: u29, len_align: u29, return_address: usize) error{OutOfMemory}![]u8 {
    var ptr = try alignedAlloc(len, alignment, len_align, return_address);
    if (len_align == 0) {
        return ptr[0..len];
    }
    return ptr[0..std.mem.alignBackwardAnyAlign(len, len_align)];
}

fn large_beam_resize(
    _: *anyopaque,
    buf: []u8,
    buf_align: u29,
    new_len: usize,
    len_align: u29,
    _: usize,
) ?usize {
  if (new_len > buf.len) { return null; }
  if (new_len == 0) { return alignedFree(buf, buf_align); }
  if (len_align == 0) { return new_len; }
  return std.mem.alignBackwardAnyAlign(new_len, len_align);
}

fn alignedAlloc(len: usize, alignment: u29, _: u29, _: usize) ![*]u8 {
  var safe_len = safeLen(len, alignment);
  var alloc_slice: []u8 = try allocator.allocAdvanced(u8, MAX_ALIGN, safe_len, std.mem.Allocator.Exact.exact);

  const unaligned_addr = @ptrToInt(alloc_slice.ptr);
  const aligned_addr = reAlign(unaligned_addr, alignment);

  getPtrPtr(aligned_addr).* = unaligned_addr;
  return aligned_addr;
}

fn alignedFree(buf: []u8, alignment: u29) usize {
  var ptr = getPtrPtr(buf.ptr).*;
  allocator.free(@intToPtr([*]u8, ptr)[0..safeLen(buf.len, alignment)]);
  return 0;
}

fn reAlign(unaligned_addr: usize, alignment: u29) [*]u8 {
  return @intToPtr(
    [*]u8,
    std.mem.alignForward(
      unaligned_addr + @sizeOf(usize),
      alignment));
}

fn safeLen(len: usize, alignment: u29) usize {
  return len + alignment - @sizeOf(usize) + MAX_ALIGN;
}

fn getPtrPtr(aligned_ptr: [*]u8) *usize {
  return @intToPtr(*usize, @ptrToInt(aligned_ptr) - @sizeOf(usize));
}

/// !value
/// wraps the zig GeneralPurposeAllocator into the standard BEAM allocator.
var general_purpose_allocator_instance = std.heap.GeneralPurposeAllocator(
.{.thread_safe = true}) {
  .backing_allocator = large_allocator,
};

pub var general_purpose_allocator = general_purpose_allocator_instance.allocator();

///////////////////////////////////////////////////////////////////////////////
// syntactic sugar: important elixir terms
///////////////////////////////////////////////////////////////////////////////

/// errors for nif translation
pub const Error = error{
  /// Translates to Elixir `FunctionClauseError`.
  ///
  /// This is the default mechanism for reporting that a Zigler nif function has
  /// been incorrectly passed a value from the Elixir BEAM runtime.  This is very
  /// important, as Zig is statically typed.
  ///
  /// support for users to be able to throw this value in their own Zig functions
  /// is forthcoming.
  FunctionClauseError,
};

/// errors for launching nif errors
/// LaunchError Occurs when there's a problem launching a threaded nif.
pub const ThreadError = error {
  LaunchError
};

/// syntactic sugar for the BEAM environment.  Note that the `env` type
/// encapsulates the pointer, since you will almost always be passing this
/// pointer to an opaque struct around without accessing it.
pub const env = ?*e.ErlNifEnv;

/// syntactic sugar for the BEAM term struct (`e.ErlNifTerm`)
pub const term = e.ErlNifTerm;

///////////////////////////////////////////////////////////////////////////////
// syntactic sugar: gets
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// generics

pub const get = @import("get.zig").get;

///////////////////////////////////////////////////////////////////////////////
// atoms

/// note that Zig has no equivalent of a BEAM atom, so we will just declare
/// it as a term.  You can retrieve the string value of the BEAM atom using
/// `get_atom_slice/2`
pub const atom = term;

const __latin1 = e.ERL_NIF_LATIN1;

/// Takes a BEAM atom term and retrieves it as a slice `[]u8` value.
/// it's the caller's responsibility to make sure that the value is freed.
///
/// Uses the standard `beam.allocator` allocator.  If you require a custom
/// allocator, use `get_atom_slice_alloc/3`
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:atom/0`
pub fn get_atom_slice(environment: env, src_term: atom) ![]u8 {
  return get_atom_slice_alloc(allocator, environment, src_term);
}

/// Takes a BEAM atom term and retrieves it as a slice `[]u8` value, with
/// any allocator.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:atom/0`
pub fn get_atom_slice_alloc(a: Allocator, environment: env, src_term: atom) ![]u8 {
  var len: c_uint = undefined;
  var result: []u8 = undefined;
  if (0 != e.enif_get_atom_length(environment, src_term, @ptrCast([*c]c_uint, &len), __latin1)) {
    result = try a.alloc(u8, len + 1);

    // pull the value from the beam.
    if (0 != e.enif_get_atom(environment, src_term, @ptrCast([*c]u8, &result[0]), len + 1, __latin1)) {
      // trim the slice, it's the caller's responsibility to free it.
      return result[0..len];
    } else { unreachable; }
  } else { return Error.FunctionClauseError; }
}

///////////////////////////////////////////////////////////////////////////////
// binaries

/// shorthand for `e.ErlNifBinary`.
pub const binary = e.ErlNifBinary;

/// Takes an BEAM `t:binary/0` term and retrieves a pointer to the
/// binary data as a Zig c-string (`[*c]u8`).  No memory is allocated for
/// this operation.
///
/// Should only be used for c interop functions.
///
/// *Note*: this function could have unexpected results if your BEAM binary
/// contains any zero byte values.  Always use `get_char_slice/2` when
/// C-interop is not necessary.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:binary/0`
pub fn get_c_string(environment: env, src_term: term) ![*c]u8 {
  var bin: binary = undefined;
  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin.data;
  } else { return Error.FunctionClauseError;}
}

/// Takes an BEAM `t:binary/0` term and retrieves it as a Zig character slice
/// (`[]u8`)  No memory is allocated for this operation.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:binary/0`
pub fn get_char_slice(environment: env, src_term: term) ![]u8 {
 var bin: binary = undefined;

  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin.data[0..bin.size];
  } else { return Error.FunctionClauseError; }
}

/// Takes an BEAM `t:binary/0` term and returns the corresponding
/// `binary` struct.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:binary/0`
pub fn get_binary(environment: env, src_term: term) !binary {
  var bin: binary = undefined;
  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin;
  } else { return Error.FunctionClauseError; }
}

///////////////////////////////////////////////////////////////////////////////
// pids

/// shorthand for `e.ErlNifPid`.
pub const pid = e.ErlNifPid;

/// Takes an BEAM `t:pid/0` term and returns the corresponding `pid`
/// struct.
///
/// Note that this is a fairly opaque struct and you're on your
/// own as to what you can do with this (for now), except as a argument
/// for the `e.enif_send` function.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:pid/0`
pub fn get_pid(environment: env, src_term: term) !pid {
  var result: pid = undefined;
  if (0 != e.enif_get_local_pid(environment, src_term, &result)) {
    return result;
  } else { return Error.FunctionClauseError; }
}

/// shortcut for `e.enif_self`, marshalling into zig error style.
///
/// returns the pid value if it's env is a process-bound environment, otherwise
/// returns `beam.Error.FunctionClauseError`.
///
/// if you're in a threaded nif, it returns the correct `self` for the process
/// running the wrapped function.  That way, `beam.self()` is safe to use when
/// you swap between different execution modes.
///
/// if you need the process mailbox for the actual spawned thread, use `e.enif_self`
pub threadlocal var self: fn (env) Error!pid = generic_self;

pub fn set_generic_self() void {
    self = generic_self;
}

fn generic_self(environment: env) !pid {
  var p: pid = undefined;
  if (e.enif_self(environment, @ptrCast([*c] pid, &p))) |self_val| {
    return self_val.*;
  } else {
    return Error.FunctionClauseError;
  }
}

// internal-use only.
///pub fn set_threaded_self() void {self = threaded_self;}
///
///fn threaded_self(environment: env) !pid {
///  if (environment == yield_info.?.environment) {
///    return yield_info.?.parent;
///  }
///  return generic_self(environment);
///}

/// shortcut for `e.enif_send`
///
/// returns true if the send is successful, false otherwise.
///
/// NOTE this function assumes a valid BEAM environment.  If you have spawned
/// an OS thread without a BEAM environment, you must use `send_advanced/4`
pub fn send(c_env: env, to_pid: pid, msg: term) bool {
  return (e.enif_send(c_env, &to_pid, null, msg) == 1);
}

/// shortcut for `e.enif_send`
///
/// returns true if the send is successful, false otherwise.
///
/// if you are sending from a thread that does not have a BEAM environment, you
/// should put `null` in both environment variables.
pub fn send_advanced(c_env: env, to_pid: pid, m_env: env, msg: term) bool {
  return (e.enif_send(c_env, &to_pid, m_env, msg) == 1);
}

///////////////////////////////////////////////////////////////////////////////
// tuples

/// Takes an Beam `t:tuple/0` term and returns it as a slice of `term` structs.
/// Does *not* allocate memory for this operation.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:tuple/0`
pub fn get_tuple(environment: env, src_term: term) ![]term {
  var length: c_int = 0;
  var term_list: [*c]term = null;
  if (0 != e.enif_get_tuple(environment, src_term, &length, &term_list)) {
    return term_list[0..(length - 1)];
  } else {return Error.FunctionClauseError; }
}

///////////////////////////////////////////////////////////////////////////////
// lists

/// Takes a BEAM `t:list/0` term and returns its length.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:list/0`
pub fn get_list_length(environment: env, list: term) !usize {
  var result: c_uint = undefined;
  if (0 != e.enif_get_list_length(environment, list, &result)) {
    return @intCast(usize, result);
  } else { return Error.FunctionClauseError; }
}

/// Iterates over a BEAM `t:list/0`.
///
/// In this function, the `list` value will be modified to the `tl` of the
/// BEAM list, and the return value will be the BEAM term.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:list/0`
pub fn get_head_and_iter(environment: env, list: *term) !term {
  var head: term = undefined;
  if (0 != e.enif_get_list_cell(environment, list.*, &head, list)) {
    return head;
  } else { return Error.FunctionClauseError; }
}

/// A generic function which lets you convert a BEAM `t:list/0` of
/// homogeous type into a Zig slice.
///
/// The resulting slice will be allocated using the beam allocator, with
/// ownership passed to the caller.  If you need to use a different allocator,
/// use `get_slice_of_alloc/4`
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:list/0`.
/// Also raises `beam.Error.FunctionClauseError` if any of the terms is
/// incompatible with the internal type
///
/// supported internal types:
/// - `c_int`
/// - `c_long`
/// - `isize`
/// - `usize`
/// - `u8`
/// - `i32`
/// - `i64`
/// - `f16`
/// - `f32`
/// - `f64`
pub fn get_slice_of(comptime T: type, environment: env, list: term) ![]T {
  return get_slice_of_alloc(T, allocator, environment, list);
}

/// Converts an BEAM `t:list/0` of homogeneous type into a Zig slice, but
/// using any allocator you wish.
///
/// ownership is passed to the caller.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:list/0`.
/// Also raises `beam.Error.FunctionClauseError` if any of the terms is
/// incompatible with the internal type.
///
/// supported internal types:
/// - `c_int`
/// - `c_long`
/// - `isize`
/// - `usize`
/// - `u8`
/// - `i32`
/// - `i64`
/// - `f16`
/// - `f32`
/// - `f64`
pub fn get_slice_of_alloc(comptime T: type, a: Allocator, environment: env, list: term) ![]T {
  const size = try get_list_length(environment, list);

  var idx: usize = 0;
  var head: term = undefined;

  // allocate memory for the Zig list.
  var result = try a.alloc(T, size);
  var movable_list = list;

  while (idx < size){
    head = try get_head_and_iter(environment, &movable_list);
    result[idx] = try get(T, environment, head);
    idx += 1;
  }
  errdefer a.free(result);

  return result;
}

///////////////////////////////////////////////////////////////////////////////
// booleans

/// private helper string comparison function
fn str_cmp(comptime ref: []const u8, str: []const u8) bool {
  if (str.len != ref.len) { return false; }
  for (str) |item, idx| {
    if (item != ref[idx]) {
      return false;
    }
  }
  return true;
}

const true_slice = "true"[0..];
const false_slice = "false"[0..];
/// Converts an BEAM `t:boolean/0` into a Zig `bool`.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:boolean/0`.
/// May potentially raise an out of memory error, as it must make an allocation
/// to perform its conversion.
pub fn get_bool(environment: env, val: term) !bool {
  var str: []u8 = undefined;
  str = try get_atom_slice(environment, val);
  defer allocator.free(str);

  if (str_cmp(true_slice, str)) {
    return true;
  } else if (str_cmp(false_slice, str)) {
    return false;
  } else {
    return Error.FunctionClauseError;
  }
}

pub const make = @import("make.zig").make;

/// !value
/// you can use this value to access the BEAM environment of your unit test.
pub threadlocal var test_env: env = undefined;

///////////////////////////////////////////////////////////////////////////////
// NIF LOADING Boilerplate

pub export fn blank_load(
  _: env,
  _: [*c]?*anyopaque,
  _: term) c_int {
  return 0;
}

pub export fn blank_upgrade(
  _: env,
  _: [*c]?*anyopaque,
  _: [*c]?*anyopaque,
  _: term) c_int {
    return 0;
}
