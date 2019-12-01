const e = @import("erl_nif.zig").c;
const std = @import("std");
const builtin = @import("builtin");

///////////////////////////////////////////////////////////////////////////////
// beam allocator definitions
///////////////////////////////////////////////////////////////////////////////

const Allocator = std.mem.Allocator;

// basic allocator

pub const allocator = &allocator_state;
var allocator_state = Allocator{
  .reallocFn = beam_realloc,
  .shrinkFn = beam_shrink
};

fn beam_realloc(self: *Allocator,
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
fn beam_shrink(self: *Allocator,
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

///////////////////////////////////////////////////////////////////////////////
// syntactic sugar: important elixir terms
///////////////////////////////////////////////////////////////////////////////
 
/// Translates to elixir FunctionClauseError
pub const Error = error {
  FunctionClauseError
};

/// Translates to AssertionError, for zigtest unit tests
pub const AssertionError = error {
  AssertionError
};

// env
pub const env = ?*e.ErlNifEnv;

// terms
pub const term = e.ErlNifTerm;

///////////////////////////////////////////////////////////////////////////////
// syntactic sugar: gets
///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////////////////////
// generics

/// a helper for zig if you want use zig's generics support to switch on
/// certain types.
pub fn get_term(comptime T: type, environment: env, value: term) !T {
  switch (T) {
    c_int  => return get_c_int(environment, value),
    c_long => return get_c_long(environment, value),
    u8     => return get_u8(environment, value),
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

/// takes a beam int term and returns a `c_int` value.  Should only be used for 
/// C interop with zig functions.
pub fn get_c_int(environment: env, src_term: term) !c_int {
  var res: c_int = undefined;
  if (0 != e.enif_get_int(environment, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

/// takes a beam int term and returns a `c_long` value.  Should only be used 
/// for C interop with zig functions.
pub fn get_c_long(environment: env, src_term: term) !c_long {
  var res: c_long = undefined;
  if (0 != e.enif_get_long(environment, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

/// takes a beam int term and returns a `u8` value.  Note that this conversion
/// function checks to make sure it's in range.
pub fn get_u8(environment: env, src_term: term) !u8 {
  var res: c_int = undefined;
  if (0 != e.enif_get_int(environment, src_term, &res)) {
    if ((res >= 0) and (res <= 255)) { 
      return @intCast(u8, res);
    } else { return Error.FunctionClauseError; } 
  } else { return Error.FunctionClauseError; }
}

/// takes a beam int term and returns a `i32` value.
pub fn get_i32(environment: env, src_term: term) !i32 {
  var res: c_int = undefined;
  if (0 != e.enif_get_int(environment, src_term, &res)) {
    return @intCast(i32, res);
  } else { return Error.FunctionClauseError; }
}

/// takes a beam int term and returns a `i64` value.
pub fn get_i64(environment: env, src_term: term) !i64 {
  var res: i64 = undefined;
  if (0 != e.enif_get_long(environment, src_term, @ptrCast(*c_long, &res))) {
    return res;
  } else { return Error.FunctionClauseError; }
}

// floats
/// takes a beam float term and returns a `f16` value.
pub fn get_f16(environment: env, src_term: term) !f16 {
  var res: f64 = undefined;
  if (0 != e.enif_get_double(environment, src_term, &res)) {
    return @floatCast(f16, res);
  } else { return Error.FunctionClauseError; }
}

/// takes a beam float term and returns a `f32` value.
pub fn get_f32(environment: env, src_term: term) !f32 {
  var res: f64 = undefined;
  if (0 != e.enif_get_double(environment, src_term, &res)) {
    return @floatCast(f32, res);
  } else { return Error.FunctionClauseError; }
}

/// takes a beam float term and returns a `f64` value.
pub fn get_f64(environment: env, src_term: term) !f64 {
  var res: f64 = undefined;
  if (0 != e.enif_get_double(environment, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

// atoms

/// note that zig has no equivalent of a beam atom, so we will just declare
/// it as a term.  You can retrieve the string value of the beam atom using
/// `get_atom_slice/2`
pub const atom = term;

const __latin1 = e.ErlNifCharEncoding.ERL_NIF_LATIN1;

/// takes a beam atom term and retrieves it as a slice `[]u8` value
/// it's the caller's responbility to make sure that the value is freed.
pub fn get_atom_slice(environment: env, src_term: atom) ![]u8 {
  var len: c_uint = undefined;
  var res: []u8 = undefined;
  if (0 != e.enif_get_atom_length(environment, src_term, @ptrCast([*c]c_uint, &len), __latin1)) {
    res = try allocator.alloc(u8, len + 1);

    // pull the value from the beam.
    if (0 != e.enif_get_atom(environment, src_term, @ptrCast([*c]u8, &res[0]), len + 1, __latin1)) {
      // trim the slice, it's the caller's responsibility to free it.
      return res[0..len];
    } else { unreachable; }
  } else { return Error.FunctionClauseError; }
}

// binaries

/// encapsulates the basic binary form, for non-automatic conversion to slices.
pub const binary = e.ErlNifBinary;

/// retrieves an erlang binary term and returns it as a zig c-string (`[*c]u8`).
/// Should only be used for c interop functions.  Consider using 
/// `get_char_slice/2` instead.
pub fn get_c_string(environment: env, src_term: term) ![*c]u8 {
  var bin: binary = undefined;
  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin.data;
  } else { return Error.FunctionClauseError;}
}

/// retrieves an erlang binary term and returns it as a zig character slice
/// (`[]u8`) NB: the calling function should NOT free the binary data from this
/// caller as it is taken care of by the BEAM.
pub fn get_char_slice(environment: env, src_term: term) ![]u8 {
  var bin: binary = undefined;
  var res: []u8 = undefined;

  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin.data[0..bin.size];
  } else { return Error.FunctionClauseError; }
}

/// retrieves an erlang binary term and returns it as a `beam.binary` term.
pub fn get_binary(environment: env, src_term: term) !binary {
  var bin: binary = undefined;
  if (0 != e.enif_inspect_binary(environment, src_term, &bin)) {
    return bin;
  } else { return Error.FunctionClauseError; }
}

// pids
pub const pid = e.ErlNifPid;

/// retrieves an erlang pid term.  Note that this is a fairly opaque structure
/// and you're on your own as to what you can do with this (for now).
pub fn get_pid(environment: env, src_term: term) !pid {
  var res: pid = undefined;
  if (0 != e.enif_get_local_pid(environment, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

// tuples

/// retrieves an erlang tuple as a slice of erlang terms.
pub fn get_tuple(environment: env, src_term: term) ![]term {
  var length: c_int;
  var term_list: [*c]term;
  if (0 != enif_get_tuple(env, src_term, &length, &term_list)) {
    return term_list[0..(length - 1)];
  } else {return Error.FunctionClauseError; }
}

// lists

/// retrieves the length of an erlang list.
pub fn get_list_length(environment: env, list: term) !usize {
  var res: c_uint = undefined;
  if (0 != e.enif_get_list_length(environment, list, &res)) {
    return @intCast(usize, res);
  } else { return Error.FunctionClauseError; }
}

/// performs erlang-style iteration on a linked list.
pub fn get_head_and_iter(environment: env, list: *term) !term {
  var head: term = undefined;
  if (0 != e.enif_get_list_cell(environment, list.*, &head, list)) {
    return head;
  } else { return Error.FunctionClauseError; }
}

/// generic function which lets you convert an erlang list into a zig list.  
/// The resulting slice will be allocated using the beam allocator, and it is
/// the responsibility of the caller to deallocate it.
pub fn get_slice_of(comptime T: type, environment: env, list: term) ![]T {
  const size = try get_list_length(environment, list);

  var idx: usize = 0;
  var head: term = undefined;

  // allocate memory for the zig list.
  var res = try allocator.alloc(T, size);
  var movable_list = list;

  while (idx < size){
    head = try get_head_and_iter(environment, &movable_list);
    res[idx] = try get_term(T, environment, head);
    idx += 1;
  }

  return res;
}

pub fn str_cmp(comptime ref: []const u8, str: []u8) bool {
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

// booleans
pub fn get_bool(environment: env, val: term) !bool {
  var str: []u8 = undefined;
  str = try get_atom_slice(environment, val);
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

// generic
pub fn make(comptime T: term, environment: env, val: T) term {
  switch (t) {
    u8     => make_char(environment, val),
    c_int  => make_c_int(environment, val),
    c_long => make_c_long(environment, val),
    i32    => make_i32(environment, val),
    i64    => make_i64(environment, val),
    f16    => make_f16(environment, val),
    f32    => make_f32(environment, val),
    f64    => make_f64(environment, val),
  }
}

// chars 
pub fn make_u8(environment: env, chr: u8) term {
  return e.enif_make_int(environment, @intCast(c_int, chr));
}

// integers
pub fn make_c_int(environment: env, val: c_int) term {
  return e.enif_make_int(environment, val);
}

pub fn make_c_long(environment: env, val: c_long) term {
  return e.enif_make_long(environment, val);
}

pub fn make_i32(environment: env, val: i32) term {
  return e.enif_make_int(environment, @intCast(c_int, val));
}

pub fn make_i64(environment: env, val: i64) term {
  return e.enif_make_long(environment, @intCast(c_long, val));
}

// floats

pub fn make_f16(environment: env, val: f16) term {
  return e.enif_make_double(environment, @floatCast(f64, val));
}

pub fn make_f32(environment: env, val: f32) term {
  return e.enif_make_double(environment, @floatCast(f64, val));
}

pub fn make_f64(environment: env, val: f64) term {
  return e.enif_make_long(environment, val);
}

// atoms

pub fn make_atom(environment: env, atom_str: []const u8) term {
  return e.enif_make_atom_len(environment, @ptrCast([*c]const u8, &atom_str[0]), atom_str.len);
}

// binaries

pub fn make_slice(environment: env, val: []u8) term {
  var res: e.ErlNifTerm = undefined;

  var bin: [*]u8 = @ptrCast([*]u8, e.enif_make_new_binary(environment, val.len, &res));

  for (result) | _chr, i | {
    bin[i] = val[i];
  }

  return res;
}

pub fn make_c_string(environment: env, val: [*c]u8) term{
  var res: e.ErlNifTerm = undefined;
  var len: usize = 0;

  // first get the length of the c string.
  for (result) | chr, i | {
    if (chr == 0) { break; }
    len = i;
  }

  // punt to the slicing function.
  return make_slice(environment, val[0..len + 1]);
}

// pids

pub fn make_pid(environment: env, val: pid) term {
  return e.enif_make_pid(environment, &pid);
}

// tuples

pub fn make_tuple(environment: env, val: []term) term {
  return e.enif_make_tuple_from_array(environment, val, val.len);
}

// lists

pub fn make_term_list(environment: env, val: []term) term {
  return e.enif_make_list_from_array(environment, val, val.len);
}

pub fn make_charlist(environment: env, val: []u8) term {
  return e.enif_make_string_len(environment, val, val.len, __latin1);
}

pub fn make_cstring_charlist(environment: env, val: [*c]u8) term {
  return e.enif_make_string(environment, val, __latin1);
}

// list-generic

pub fn make_list(comptime T: term, environment, env, val: []T) term {
  var term_slice: []term = allocator.alloc(term, val.len);
  defer allocator.free(term_slice);

  for (val) | item, idx | {
    term_slice[idx] = make(T, environment, item);
  }

  return e.enif_make_list_from_array(environment, term_slice, val.len);
}

pub fn make_c_int_list(environment: env, val: []c_int) term {
  return make_list(c_int, environment, val);
}

pub fn make_c_long_list(environment: env, val: []c_long) term {
  return make_list(c_long, environment, val);
}

pub fn make_i32_list(environment: env, val: []i32) term {
  return make_list(i32, environment, val);
}

pub fn make_i64_list(environment: env, val: []i64) term {
  return make_list(i64, environment, val);
}

pub fn make_f16_list(environment: env, val: []f16) term {
  return make_list(i64, environment, val);
}

pub fn make_f32_list(environment: env, val: []f32) term {
  return make_list(i64, environment, val);
}

pub fn make_f64_list(environment: env, val: []f64) term {
  return make_list(i64, environment, val);
}

pub fn make_bool(environment: env, val: bool) term {
  return if (val) e.enif_make_atom(env, c"true") else e.enif_make_atom(env, c"false");
}

// implementation for :enomem

// create a global enomem string, then throw it.
const enomem_slice = "enomem"[0..];

/// The BEAM is potentially OOM-safe, and zig lets you really leverage that.
/// this communicates back to the BEAM that you've attempted to allocate more
/// memory than is available via an allocation event.
pub fn throw_enomem(environment: env) term {
  return e.enif_raise_exception(environment, make_atom(environment, enomem_slice));
}

const f_c_e_slice = "function_clause"[0..];

/// A function to declare that you've passed a horrible value to one of your 
/// nif functions.  By default zigler will do parameter input checking on value
/// ingress from the dynamic BEAM runtime to the static zig runtime.  This
/// communicates back to the beam that this has happened.
pub fn throw_function_clause_error(environment: env) term {
  return e.enif_raise_exception(environment, make_atom(environment, f_c_e_slice));
}

const assert_slice = "assertion_error"[0..];

/// when running zigtests, a beam.AssertionError.AssertionError value gets trapped
/// using this function, which communicates it back to the BEAM as an exception.
pub fn throw_assertion_error(environment: env) term {
  return e.enif_raise_exception(environment, make_atom(environment, assert_slice));
}

/// When building zigtests, `assert(...)` calls get lexically converted to 
/// `beam.assert(...)` calls.  Zig's std.assert() will panic the zig runtime and
/// therefore the entire BEAM VM, which makes it incompatible with Elixir's Unit
/// tests.
pub fn assert(ok: bool) !void {
    if (!ok) return AssertionError.AssertionError; // assertion failure
}