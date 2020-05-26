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
//! Beacuse Zig features *composable allocators*, you can very easily implement
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

///////////////////////////////////////////////////////////////////////////////
// BEAM allocator definitions
///////////////////////////////////////////////////////////////////////////////

const Allocator = std.mem.Allocator;

// basic allocator

/// !value
/// provides a default BEAM allocator.  Use `beam.allocator.alloc` everywhere
/// to safely allocate memory efficiently, and use `beam.allocator.free` to
/// release that memory.
///
/// Note this does not make the allocated memory *garbage collected* by the
/// BEAM.
///
/// ### Example
///
/// The following code will return ten bytes of new memory.
///
/// ```
/// const beam = @import("beam.zig");
///
/// fn give_me_ten_bytes() ![]u8 {
///   return beam.allocator.alloc(u8, 10);
/// }
/// ```
pub threadlocal var allocator: *Allocator = &beam_allocator;

var beam_allocator = Allocator{
  .reallocFn = beam_realloc,
  .shrinkFn = beam_shrink
};

fn beam_realloc(_self: *Allocator,
                old_mem: []u8,
                old_align: u29,
                new_size: usize,
                new_align: u29) ![]u8 {
  if (old_mem.len == 0) {
    // if we're creating a new memory space, use alloc.
    const buf = e.enif_alloc(new_size) orelse return error.OutOfMemory;
    return @ptrCast([*]u8, buf)[0..new_size];
  } else {
    // if we're actually resizing a memory space, use realloc.
    const old_ptr = @ptrCast(*c_void, old_mem.ptr);
    const buf = e.enif_realloc(old_ptr, new_size) orelse return error.OutOfMemory;
    return @ptrCast([*]u8, buf)[0..new_size];
  }
}

var nothing = [_]u8 {};
fn beam_shrink(_self: *Allocator,
               old_mem: []u8,
               old_align: u29,
               new_size: usize,
               new_align: u29) []u8 {
  if (new_size == 0) {
    e.enif_free(@ptrCast(*c_void, old_mem.ptr));
    return nothing[0..0];
  } else {
    // if we're actually resizing a memory space, use realloc.
    const old_ptr = @ptrCast(*c_void, old_mem.ptr);
    const buf = e.enif_realloc(old_ptr, new_size) orelse return old_mem[0..new_size];
    return @ptrCast([*]u8, buf)[0..new_size];
  }
}

/// creates a specialized auto-allocator which manages memory on behalf of
/// the thread and can clean up after a thread, so that memory management is not
/// necessary for this nif function call.
pub const AutoAllocator = struct {
  // members
  allocator: *Allocator,
  buffer_list: std.LinkedList([]u8),

  const BufNode = std.LinkedList([]u8).Node;
  pub fn init() AutoAllocator {
    return AutoAllocator{
      .allocator = Allocator{
          .reallocFn = realloc,
          .shrinkFn = shrink,
      },
      .buffer_list = std.LinkedList([]u8).init(),
    };
  }

  pub fn deinit(self: *AutoAllocator) void {
    var node_or_null = self.buffer_list.first;
    while (node_or_null) |node| {
      // this has to occur before the free because the free frees node
      node_or_null = node.next;
      allocator.free(node.data);
      // destroy the node.
      std.LinkedList.destroyNode(self.buffer_list, node, allocator);
    }
  }

  fn realloc(self_generic: *Allocator,
             old_mem: []u8,
             old_align: u29,
             new_size: usize,
             new_align: u29) ![]u8 {

    const self = @fieldParentPtr(ThreadAllocator, "allocator", self_generic);

    if (old_mem.len == 0) {
      // if we're creating a new memory space, use basic beam alloc.
      const buf = e.enif_alloc(new_size) orelse return error.OutOfMemory;
      // then prepend it to the linked list.
      do_prepend(self, buf);

      return @ptrCast([*]u8, buf)[0..new_size];
    } else {
      // if we're actually resizing a memory space, use realloc.
      const old_ptr = @ptrCast(*c_void, old_mem.ptr);
      const buf = e.enif_realloc(old_ptr, new_size) orelse return error.OutOfMemory;

      // remove the old element of the buffer.
      self.buffer_list.remove(old_mem);
      // put the new buffer in there.
      var new_buf = @ptrCast([*]u8, buf)[0..new_size];

      do_prepend(self, new_buf);

      // return the new slice.
      return new_buf;
    }
  }

  fn shrink(self_generic: *Allocator,
               old_mem: []u8,
               old_align: u29,
               new_size: usize,
               new_align: u29) []u8 {
    const self = @fieldParentPtr(ThreadAllocator, "allocator", self_generic);
    if (new_size == 0) {
      // use the beam engine to free the memory slice.
      e.enif_free(@ptrCast(*c_void, old_mem.ptr));

      // remove it from the linked list.
      std.LinkedList.do_remove(self, old_mem);

      // send back a generic empty slice.
      return []u8{};
    } else {
      // if we're actually resizing a memory space, use realloc.
      const old_ptr = @ptrCast(*c_void, old_mem.ptr);
      const buf = e.enif_realloc(old_ptr, new_size) orelse return error.OutOfMemory;

      // remove the old element of the buffer.
      self.buffer_list.remove(old_mem);
      // put the new buffer in there.
      var new_buf = @ptrCast([*]u8, buf)[0..new_size];

      do_prepend(self, new_buf);

      // return the new slice.
      return new_buf;
    }
  }

  fn do_prepend(self: *ThreadAllocator, buf: []u8) ![]u8 {
    // use the beam allocator to allocate a new memory node.
    var new_node = try std.LinkedList.allocateNode(self.buffer_list, allocator);
    // store the buffer into the node
    new_node.data = buf;
    // append the buffer value to the node.
    std.LinkedList.prepend(self.buffer_list, buf);
    // return the newly allocated memory
    return buf;
  }

  fn do_remove(self: *ThreadAllocator, buf: []u8) void {
    // find the node where buf is.
    var current_node: *std.LinkedList.Node = self.first
      orelse unreachable;
    while (current_node.data != buf) {
      current_node = current_node.next
        orelse unreachable;
    }
    // remove it.
    std.LinkedList.remove(self.buffer_list, current_node);
    // free the buffer node
    std.LinkedList.destroyNode(self.buffer_list, current_node, allocator);
  }
};

///////////////////////////////////////////////////////////////////////////////
// syntactic sugar: important elixir terms
///////////////////////////////////////////////////////////////////////////////

/// errors for nif translation
pub const Error = error {
  /// Translates to Elixir `FunctionClauseError`.
  ///
  /// This is the default mechanism for reporting that a Zigler nif function has
  /// been incorrectly passed a value from the Elixir BEAM runtime.  This is very
  /// important, as Zig is statically typed.
  ///
  /// support for users to be able to throw this value in their own Zig functions
  /// is forthcoming.
  FunctionClauseError
};

/// errors for testing
pub const AssertionError = error {
  /// Translates to `ExUnit.AssertionError`.  Mostly used in Zig unit tests.
  ///
  /// All test clauses in the directories of your Zig-enabled modules are
  /// converted to Zig functions with the inferred type `!void`.  The
  /// `beam.assert/1` function can throw this error as its error type.
  ///
  /// Zigler converts assert statements in test blocks to `try beam.assert(...);`
  AssertionError
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

/// A helper for marshalling values from the BEAM runtime into Zig.  Use this
/// function if you need support for Zig generics.
///
/// Used internally to typcheck values coming into Zig slice.
///
/// supported types:
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
pub fn get(comptime T: type, environment: env, value: term) !T {
  switch (T) {
    c_int  => return get_c_int(environment, value),
    c_long => return get_c_long(environment, value),
    isize  => return get_isize(environment, value),
    usize  => return get_usize(environment, value),
    u8     => return get_u8(environment, value),
    u16    => return get_u16(environment, value),
    u32    => return get_u32(environment, value),
    u64    => return get_u64(environment, value),
    i32    => return get_i32(environment, value),
    i64    => return get_i64(environment, value),
    f16    => return get_f16(environment, value),
    f32    => return get_f32(environment, value),
    f64    => return get_f64(environment, value),
    else   => unreachable
  }
}

///////////////////////////////////////////////////////////////////////////////
// ints

/// Takes a BEAM int term and returns a `c_int` value.  Should only be used for
/// C interop with Zig functions.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_c_int(environment: env, src_term: term) !c_int {
  var result: c_int = undefined;
  if (0 != e.enif_get_int(environment, src_term, &result)) {
    return result;
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `c_uint` value.  Should only be used for
/// C interop with Zig functions.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_c_uint(environment: env, src_term: term) !c_uint {
  var result: c_uint = undefined;
  if (0 != e.enif_get_uint(environment, src_term, &result)) {
    return result;
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `c_long` value.  Should only be used
/// for C interop with Zig functions.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_c_long(environment: env, src_term: term) !c_long {
  var result: c_long = undefined;
  if (0 != e.enif_get_long(environment, src_term, &result)) {
    return result;
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `c_ulong` value.  Should only be used
/// for C interop with Zig functions.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_c_ulong(environment: env, src_term: term) !c_ulong {
  var result: c_ulong = undefined;
  if (0 != e.enif_get_ulong(environment, src_term, &result)) {
    return result;
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `isize` value.  Should only be used
/// for C interop.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_isize(environment: env, src_term: term) !isize {
  var result: i64 = undefined;
  if (0 != e.enif_get_long(environment, src_term, @ptrCast(*c_long, &result))) {
    return @intCast(isize, result);
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `usize` value.  Zig idiomatically uses
/// `usize` for its size values, so typically you should be using this function.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_usize(environment: env, src_term: term) !usize {
  var result: i64 = undefined;
  if (0 != e.enif_get_long(environment, src_term, @ptrCast(*c_long, &result))) {
    return @intCast(usize, result);
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `u8` value.
///
/// Note that this conversion function checks to make sure it's in range
/// (`0..255`).
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_u8(environment: env, src_term: term) !u8 {
  var result: c_int = undefined;
  if (0 != e.enif_get_int(environment, src_term, &result)) {
    if ((result >= 0) and (result <= 0xFF)) {
      return @intCast(u8, result);
    } else { return Error.FunctionClauseError; }
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `u16` value.
///
/// Note that this conversion function checks to make sure it's in range
/// (`0..65535`).
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_u16(environment: env, src_term: term) !u16 {
  var result: c_int = undefined;
  if (0 != e.enif_get_int(environment, src_term, &result)) {
    if ((result >= 0) and (result <= 0xFFFF)) {
      return @intCast(u16, result);
    } else { return Error.FunctionClauseError; }
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `u32` value.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_u32(environment: env, src_term: term) !u32 {
  var result: c_uint = undefined;
  if (0 != e.enif_get_uint(environment, src_term, &result)) {
    return @intCast(u32, result);
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns a `u64` value.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_u64(environment: env, src_term: term) !u64 {
  var result: c_ulong = undefined;
  if (0 != e.enif_get_ulong(environment, src_term, &result)) {
    return @intCast(u64, result);
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns an `i32` value.
///
/// Note that this conversion function does not currently do range checking.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_i32(environment: env, src_term: term) !i32 {
  var result: c_int = undefined;
  if (0 != e.enif_get_int(environment, src_term, &result)) {
    return @intCast(i32, result);
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM int term and returns an `i64` value.
///
/// Note that this conversion function does not currently do range checking.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:integer/0`
pub fn get_i64(environment: env, src_term: term) !i64 {
  var result: i64 = undefined;
  if (0 != e.enif_get_long(environment, src_term, @ptrCast(*c_long, &result))) {
    return result;
  } else { return Error.FunctionClauseError; }
}

///////////////////////////////////////////////////////////////////////////////
// floats

/// Takes a BEAM float term and returns an `f16` value.
///
/// Note that this conversion function does not currently do range checking.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:float/0`
pub fn get_f16(environment: env, src_term: term) !f16 {
  var result: f64 = undefined;
  if (0 != e.enif_get_double(environment, src_term, &result)) {
    return @floatCast(f16, result);
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM float term and returns an `f32` value.
///
/// Note that this conversion function does not currently do range checking.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:float/0`
pub fn get_f32(environment: env, src_term: term) !f32 {
  var result: f64 = undefined;
  if (0 != e.enif_get_double(environment, src_term, &result)) {
    return @floatCast(f32, result);
  } else { return Error.FunctionClauseError; }
}

/// Takes a BEAM float term and returns an `f64` value.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:float/0`
pub fn get_f64(environment: env, src_term: term) !f64 {
  var result: f64 = undefined;
  if (0 != e.enif_get_double(environment, src_term, &result)) {
    return result;
  } else { return Error.FunctionClauseError; }
}

///////////////////////////////////////////////////////////////////////////////
// atoms

/// note that Zig has no equivalent of a BEAM atom, so we will just declare
/// it as a term.  You can retrieve the string value of the BEAM atom using
/// `get_atom_slice/2`
pub const atom = term;

const __latin1 = e.ErlNifCharEncoding.ERL_NIF_LATIN1;

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
pub fn get_atom_slice_alloc(a: *Allocator, environment: env, src_term: atom) ![]u8 {
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

/// Takes an BEAM `t:Kernel.binary/0` term and retrieves a pointer to the
/// binary data as a Zig c-string (`[*c]u8`).  No memory is allocated for
/// this operation.
///
/// Should only be used for c interop functions.
///
/// *Note*: this function could have unexpected results if your BEAM binary
/// contains any zero byte values.  Always use `get_char_slice/2` when
/// C-interop is not necessary.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:Kernel.binary/0`
pub fn get_c_string(environment: env, src_term: term) ![*c]u8 {
  var bin: binary = undefined;
  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin.data;
  } else { return Error.FunctionClauseError;}
}

/// Takes an BEAM `t:Kernel.binary/0` term and retrieves it as a Zig character slice
/// (`[]u8`)  No memory is allocated for this operation.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:Kernel.binary/0`
pub fn get_char_slice(environment: env, src_term: term) ![]u8 {
  var bin: binary = undefined;
  var result: []u8 = undefined;

  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin.data[0..bin.size];
  } else { return Error.FunctionClauseError; }
}

/// Takes an BEAM `t:Kernel.binary/0` term and returns the corresponding
/// `binary` struct.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:Kernel.binary/0`
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

/// Takes an BEAM `t:Kernel.pid/0` term and returns the corresponding `pid`
/// struct.
///
/// Note that this is a fairly opaque struct and you're on your
/// own as to what you can do with this (for now), except as a argument
/// for the `e.enif_send` function.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:Kernel.pid/0`
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
pub fn self(environment: env) !pid {
  var p: pid = undefined;
  if (e.enif_self(environment, @ptrCast([*c] pid, &p))) |self_val| {
    return self_val.*;
  } else {
    return Error.FunctionClauseError;
  }
}

pub fn send(c_env: env, to_pid: pid, m_env: env, msg: term) bool {
  return (e.enif_send(c_env, &to_pid, m_env, msg) == 1);
}

///////////////////////////////////////////////////////////////////////////////
// tuples

/// Takes an Beam `t:tuple/0` term and returns it as a slice of `term` structs.
/// Does *not* allocate memory for this operation.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:tuple/0`
pub fn get_tuple(environment: env, src_term: term) ![]term {
  var length: c_int;
  var term_list: [*c]term;
  if (0 != enif_get_tuple(env, src_term, &length, &term_list)) {
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

/// Converts an BEAM `t:list/0` of homogenous type into a Zig slice, but
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
pub fn get_slice_of_alloc(comptime T: type, a: *Allocator, environment: env, list: term) ![]T {
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

///////////////////////////////////////////////////////////////////////////////
// syntactic sugar: makes
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// generic

/// A helper for marshalling values from Zig back into the runtime.  Use this
/// function if you need support for Zig generics.
///
/// supported types:
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
pub fn make(comptime T: type, environment: env, val: T) term {
  switch (T) {
    u8     => return make_u8(environment, val),
    u16    => return make_u16(environment, val),
    u32    => return make_u32(environment, val),
    u64     => return make_u64(environment, val),
    c_int   => return make_c_int(environment, val),
    c_uint  => return make_c_uint(environment, val),
    c_long  => return make_c_long(environment, val),
    c_ulong => return make_c_ulong(environment, val),
    isize   => return make_isize(environment, val),
    usize   => return make_usize(environment, val),
    i32     => return make_i32(environment, val),
    i64     => return make_i64(environment, val),
    f16     => return make_f16(environment, val),
    f32     => return make_f32(environment, val),
    f64     => return make_f64(environment, val),
    else    => unreachable
  }
}

/// converts a char (`u8`) value into a BEAM `t:integer/0`.
pub fn make_u8(environment: env, chr: u8) term {
  return e.enif_make_uint(environment, @intCast(c_uint, chr));
}

/// converts a unsigned (`u16`) value into a BEAM `t:integer/0`.
pub fn make_u16(environment: env, val: u16) term {
  return e.enif_make_uint(environment, @intCast(c_uint, val));
}

/// converts a unsigned (`u32`) value into a BEAM `t:integer/0`.
pub fn make_u32(environment: env, val: u32) term {
  return e.enif_make_uint(environment, @intCast(c_uint, val));
}

/// converts a unsigned (`u64`) value into a BEAM `t:integer/0`.
pub fn make_u64(environment: env, val: u64) term {
  return e.enif_make_ulong(environment, @intCast(c_ulong, val));
}

/// converts a `c_int` value into a BEAM `t:integer/0`.
pub fn make_c_int(environment: env, val: c_int) term {
  return e.enif_make_int(environment, val);
}

/// converts a `c_uint` value into a BEAM `t:integer/0`.
pub fn make_c_uint(environment: env, val: c_uint) term {
  return e.enif_make_uint(environment, val);
}

/// converts a `c_long` value into a BEAM `t:integer/0`.
pub fn make_c_long(environment: env, val: c_long) term {
  return e.enif_make_long(environment, val);
}

/// converts a `c_ulong` value into a BEAM `t:integer/0`.
pub fn make_c_ulong(environment: env, val: c_ulong) term {
  return e.enif_make_ulong(environment, val);
}

/// converts an `isize` value into a BEAM `t:integer/0`.
pub fn make_isize(environment: env, val: isize) term {
  return e.enif_make_int(environment, @intCast(c_int, val));
}

/// converts a `usize` value into a BEAM `t:integer/0`.
pub fn make_usize(environment: env, val: usize) term {
  return e.enif_make_int(environment, @intCast(c_int, val));
}

/// converts an `i32` value into a BEAM `t:integer/0`.
pub fn make_i32(environment: env, val: i32) term {
  return e.enif_make_int(environment, @intCast(c_int, val));
}

/// converts an `i64` value into a BEAM `t:integer/0`.
pub fn make_i64(environment: env, val: i64) term {
  return e.enif_make_long(environment, @intCast(c_long, val));
}

///////////////////////////////////////////////////////////////////////////////
// floats

/// converts an `f16` value into a BEAM `t:float/0`.
pub fn make_f16(environment: env, val: f16) term {
  return e.enif_make_double(environment, @floatCast(f64, val));
}

/// converts an `f32` value into a BEAM `t:float/0`.
pub fn make_f32(environment: env, val: f32) term {
  return e.enif_make_double(environment, @floatCast(f64, val));
}

/// converts an `f64` value into a BEAM `t:float/0`.
pub fn make_f64(environment: env, val: f64) term {
  return e.enif_make_double(environment, val);
}

///////////////////////////////////////////////////////////////////////////////
// atoms

/// converts a Zig char slice (`[]u8`) into a BEAM `t:atom/0`.
pub fn make_atom(environment: env, atom_str: []const u8) term {
  return e.enif_make_atom_len(environment, @ptrCast([*c]const u8, &atom_str[0]), atom_str.len);
}

///////////////////////////////////////////////////////////////////////////////
// binaries

/// converts a Zig char slice (`[]u8`) into a BEAM `t:binary/0`.
///
/// no memory allocation inside of Zig is performed and the BEAM environment
/// is responsible for the resulting binary.  You are responsible for managing
/// the allocation of the slice.
pub fn make_slice(environment: env, val: []const u8) term {
  var result: e.ErlNifTerm = undefined;

  var bin: [*]u8 = @ptrCast([*]u8, e.enif_make_new_binary(environment, val.len, &result));

  for (val) | _chr, i | {
    bin[i] = val[i];
  }

  return result;
}

/// converts an c string (`[*c]u8`) into a BEAM `t:binary/0`. Mostly used for
/// c interop.
///
/// no memory allocation inside of Zig is performed and the BEAM environment
/// is responsible for the resulting binary.  You are responsible for managing
/// the allocation of the slice.
pub fn make_c_string(environment: env, val: [*c] const u8) term {
  var result: e.ErlNifTerm = undefined;
  var len: usize = 0;

  // first get the length of the c string.
  for (result) | chr, i | {
    if (chr == 0) { break; }
    len = i;
  }

  // punt to the slicing function.
  return make_slice(environment, val[0..len + 1]);
}

///////////////////////////////////////////////////////////////////////////////
// tuples

/// converts a slice of `term`s into a BEAM `t:tuple/0`.
pub fn make_tuple(environment: env, val: []term) term {
  return e.enif_make_tuple_from_array(
    environment, @ptrCast([*c]term, val.ptr), @intCast(c_uint, val.len));
}

///////////////////////////////////////////////////////////////////////////////
// lists

/// converts a slice of `term`s into a BEAM `t:list/0`.
pub fn make_term_list(environment: env, val: []term) term {
  return e.enif_make_list_from_array(
    environment, @ptrCast([*c]term, val.ptr), @intCast(c_uint, val.len));
}

/// converts a Zig char slice (`[]u8`) into a BEAM `t:charlist/0`.
pub fn make_charlist(environment: env, val: [] const u8) term {
  return e.enif_make_string_len(environment, val, val.len, __latin1);
}

/// converts a c string (`[*c]u8`) into a BEAM `t:charlist/0`.
pub fn make_cstring_charlist(environment: env, val: [*c] const u8) term {
  return e.enif_make_string(environment, val, __latin1);
}

///////////////////////////////////////////////////////////////////////////////
// list-generic

/// A helper to make BEAM lists out of slices of `term`.  Use this function if
/// you need a generic listbuilding function.
///
/// uses the BEAM allocator internally.  If you would like to use a custom
/// allocator, (for example an arena allocator, if you have very long lists),
/// use `make_list_alloc/4`
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
pub fn make_list(comptime T: type, environment: env, val: []T) !term {
  return make_list_alloc(T, allocator, environment, val);
}

/// A helper to make a BEAM `t:Kernel.list` out of `term`s, with any allocator.
/// Use this function if you need a generic listbuilding function.
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
pub fn make_list_alloc(comptime T: type, a: *Allocator, environment: env, val: []T) !term {
  var term_slice: []term = try a.alloc(term, val.len);
  defer a.free(term_slice);

  for (val) | item, idx | {
    term_slice[idx] = make(T, environment, item);
  }

  return e.enif_make_list_from_array(environment, @ptrCast([*c]term, &term_slice[0]), @intCast(c_uint, val.len));
}

/// converts a c_int slice (`[]c_int`) into a BEAM list of `integer/0`.
pub fn make_c_int_list(environment: env, val: []c_int) !term {
  return try make_list(c_int, environment, val);
}

/// converts a c_long slice (`[]c_long`) into a BEAM list of `integer/0`.
pub fn make_c_long_list(environment: env, val: []c_long) !term {
  return try make_list(c_long, environment, val);
}

/// converts an i32 slice (`[]i32`) into a BEAM list of `integer/0`.
pub fn make_i32_list(environment: env, val: []i32) !term {
  return try make_list(i32, environment, val);
}

/// converts an i64 slice (`[]i64`) into a BEAM list of `integer/0`.
pub fn make_i64_list(environment: env, val: []i64) !term {
  return try make_list(i64, environment, val);
}

/// converts an f16 slice (`[]f16`) into a BEAM list of `t:float/0`.
pub fn make_f16_list(environment: env, val: []f16) !term {
  return try make_list(f16, environment, val);
}

/// converts an f32 slice (`[]f32`) into a BEAM list of `t:float/0`.
pub fn make_f32_list(environment: env, val: []f32) !term {
  return try make_list(f32, environment, val);
}

/// converts an f64 slice (`[]f64`) into a BEAM list of `t:float/0`.
pub fn make_f64_list(environment: env, val: []f64) !term {
  return try make_list(f64, environment, val);
}

///////////////////////////////////////////////////////////////////////////////
// booleans

/// converts a `bool` value into a `t:Kernel.boolean/0` value.
pub fn make_bool(environment: env, val: bool) term {
  return if (val) e.enif_make_atom(environment, "true") else e.enif_make_atom(environment, "false");
}

/// creates a beam `nil` value.
pub fn make_nil(environment: env) term {
  return e.enif_make_atom(environment, "nil");
}

///////////////////////////////////////////////////////////////////////////////
// ok and error tuples

/// A helper to make `{:ok, term}` terms from arbitrarily-typed values.
///
/// supported types:
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
///
/// Use `make_ok_term/2` to make ok tuples from generic terms.
/// Use `make_ok_atom/2` to make ok tuples with atom terms from slices.
pub fn make_ok_tuple(comptime T: type, environment: env, val: T) term {
  return make_ok_term(environment, make(T, environment, val));
}

/// A helper to make `{:ok, binary}` terms from slices
pub fn make_ok_binary(environment: env, val: [] const u8) term {
  return make_ok_term(environment, make_slice(environment, val));
}

/// A helper to make `{:ok, atom}` terms from slices
pub fn make_ok_atom(environment: env, val: [] const u8) term {
  return make_ok_term(environment, make_atom(environment, val));
}

/// A helper to make `{:ok, term}` terms in general
pub fn make_ok_term(environment: env, val: term) term {
  return e.enif_make_tuple(environment, 2,
    e.enif_make_atom(environment, "ok"), val);
}

/// A helper to make `{:error, term}` terms from arbitrarily-typed values.
///
/// supported types:
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
///
/// Use `make_error_term/2` to make error tuples from generic terms.
/// Use `make_error_atom/2` to make atom errors from slices.
pub fn make_error_tuple(comptime T: type, environment: env, val: T) term {
  return make_error_term(environment, make(T, environment, val));
}

/// A helper to make `{:error, atom}` terms from slices
pub fn make_error_atom(environment: env, val: [] const u8) term {
  return make_error_term(environment, make_atom(environment, val));
}

/// A helper to make `{:error, binary}` terms from slices
pub fn make_error_binary(environment: env, val: [] const u8) term {
  return make_error_term(environment, make_slice(environment, val));
}

/// A helper to make `{:error, term}` terms in general
pub fn make_error_term(environment: env, val: term) term {
  return e.enif_make_tuple(environment, 2,
    e.enif_make_atom(environment, "error"), val);
}

///////////////////////////////////////////////////////////////////////////////
// refs

/// Encapsulates `e.enif_make_ref` and allows it to return a
/// FunctionClauseError.
///
/// Raises `beam.Error.FunctionClauseError` if the term is not `t:Kernel.pid/0`
pub fn make_ref(environment: env) !term {
  var result = e.enif_make_ref(environment);
  if (0 != result) { return result; } else { return Error.FunctionClauseError; }
}

///////////////////////////////////////////////////////////////////////////////
// resources

pub const resource_type = ?*e.ErlNifResourceType;

pub const resource = struct {
  // fix this V V V
  pub const Err = error {
    /// something has gone wrong while trying to fetch a resource.
    ResourceError
  };

  pub fn create(comptime T : type, environment: env, res_typ: resource_type, val : T) !term {
    var ptr : ?*c_void = e.enif_alloc_resource(res_typ, @sizeOf(T));
    var obj : *T = undefined;

    if (ptr == null) {
      return error.enomem;
    } else {
      obj = @ptrCast(*T, @alignCast(@alignOf(*T), ptr));
      obj.* = val;
    }

    return e.enif_make_resource(environment, ptr);
  }

  pub fn update(comptime T : type, environment: env, res_typ: resource_type, res_trm: term, new_val: T) !void {
    var obj : ?*c_void = undefined;

    if (0 == e.enif_get_resource(environment, res_trm, res_typ, @ptrCast([*c]?*c_void, &obj))) {
      return resource.Err.ResourceError;
    }

    if (obj == null) { unreachable; }

    var val : *T = @ptrCast(*T, @alignCast(@alignOf(*T), obj));

    val.* = new_val;
  }

  pub fn fetch(comptime T : type, environment: env, res_typ: resource_type, res_trm: term) !T {
    var obj : ?*c_void = undefined;

    if (0 == e.enif_get_resource(environment, res_trm, res_typ, @ptrCast([*c]?*c_void, &obj))) {
      return resource.Err.ResourceError;
    }

    // according to the erlang documentation:
    // the pointer received in *objp is guaranteed to be valid at least as long as the
    // resource handle term is valid.

    if (obj == null) { unreachable; }

    var val : *T = @ptrCast(*T, @alignCast(@alignOf(*T), obj));

    return val.*;
  }

  pub fn release(environment: env, res_typ: resource_type, res_trm: term) void {
    var obj : ?*c_void = undefined;
    var _rsrc = e.enif_get_resource(environment, res_trm, res_typ, &obj);
    return e.enif_release_resource(obj);
  }
};

///////////////////////////////////////////////////////////////////////////////
// errors, etc.

pub fn raise(environment: env, exception: atom) term {
  return e.enif_raise_exception(environment, exception);
}

// create a global enomem string, then throw it.
const enomem_slice = "enomem"[0..];

/// This function is used to communicate `:enomem` back to the BEAM as an
/// exception.
///
/// The BEAM is potentially OOM-safe, and Zig lets you leverage that.
/// OOM errors from `beam.allocator` can be converted to a generic erlang term
/// that represents an exception.  Returning this from your NIF results in
/// a BEAM throw event.
pub fn raise_enomem(environment: env) term {
  return e.enif_raise_exception(environment, make_atom(environment, enomem_slice));
}

const f_c_e_slice = "function_clause"[0..];

/// This function is used to communicate `:function_clause` back to the BEAM as an
/// exception.
///
/// By default Zigler will do argument input checking on value
/// ingress from the dynamic BEAM runtime to the static Zig runtime.
/// You can also use this function to communicate a similar error by returning the
/// resulting term from your NIF.
pub fn raise_function_clause_error(environment: env) term {
  return e.enif_raise_exception(environment, make_atom(environment, f_c_e_slice));
}

const assert_slice = "assertion_error"[0..];

/// This function is used to communicate `:assertion_error` back to the BEAM as an
/// exception.
///
/// Used when running Zigtests, when trapping `beam.AssertionError.AssertionError`.
pub fn raise_assertion_error(environment: env) term {
  return e.enif_raise_exception(environment, make_atom(environment, assert_slice));
}

///////////////////////////////////////////////////////////////////////////////
// assertation for tests

/// A function used to return assertion errors to a zigtest.
///
/// Zig's std.assert() will panic the Zig runtime and therefore the entire
/// BEAM VM, making it incompatible with Elixir's Unit tests.  As the VM is
/// required for certain functionality (like `e.enif_alloc`), a BEAM-compatible
/// assert is necessary.
///
/// When building zigtests, `assert(...)` calls get lexically converted to
/// `try beam.assert(...)` calls.
pub fn assert(ok: bool, file: []const u8, line: i64) !void {
  if (!ok) {
    error_file = file;
    error_line = line;
    return AssertionError.AssertionError; // assertion failure
  }
}

/// !value
/// you can use this value to access the BEAM environment of your unit test.
pub threadlocal var test_env: env = undefined;
pub threadlocal var error_file: []const u8 = undefined;
pub threadlocal var error_line: i64 = 0;

// private function which fetches the threadlocal cache.
pub fn test_error() term {
  var tuple_slice: []term = allocator.alloc(term, 3) catch unreachable;
  defer allocator.free(tuple_slice);
  tuple_slice[0] = make_atom(test_env, "error");
  tuple_slice[1] = make_slice(test_env, error_file);
  tuple_slice[2] = make_i64(test_env, error_line);
  return make_tuple(test_env, tuple_slice);
}
