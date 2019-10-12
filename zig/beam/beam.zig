const e = @import("erl_nif.zig").c;
const std = @import("std");

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

////////////////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////////////////
// syntactic sugar
///////////////////////////////////////////////////////////////////////////////////

pub const Error = error {
    FunctionClauseError
};

// env
pub const env = ?*e.ErlNifEnv;

// terms
pub const term = e.ErlNifTerm;

// ints
pub fn get_c_int(erl_env: env, src_term: term) !c_int {
  var res: c_int = undefined;
  if (0 != e.enif_get_int(erl_env, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

pub fn get_c_long(erl_env: env, src_term: term) !c_long {
  var res: c_long = undefined;
  if (0 != e.enif_get_int(erl_env, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

pub fn get_i64(erl_env: env, src_term: term) !i64 {
  var res: i64 = undefined;
  if (0 != e.enif_get_long(erl_env, src_term, @ptrCast(*c_long, &res))) {
    return res;
  } else { return Error.FunctionClauseError; }
}

// floats
pub fn get_f16(erl_env: env, src_term: term) !f16 {
  var res: f64 = undefined;
  if (0 != e.enif_get_double(erl_env, src_term, &res)) {
    return @floatCast(f16, res);
  } else { return Error.FunctionClauseError; }
}

pub fn get_f32(erl_env: env, src_term: term) !f32 {
  var res: f64 = undefined;
  if (0 != e.enif_get_double(erl_env, src_term, &res)) {
    return @floatCast(f32, res);
  } else { return Error.FunctionClauseError; }
}

pub fn get_f64(erl_env: env, src_term: term) !f64 {
  var res: f64 = undefined;
  if (0 != e.enif_get_double(erl_env, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

// binaries
pub const binary = e.ErlNifBinary;

// lists

pub fn get_list_length(erl_env: env, list: term) !usize {
  var res: c_uint = undefined;
  if (0 != e.enif_get_list_length(erl_env, list, &res)) {
    return @intCast(usize, res);
  } else { return Error.FunctionClauseError; }
}

pub fn get_head_and_iter(erl_env: env, list: *term) !term {
  var head: term = undefined;
  if (0 != e.enif_get_list_cell(erl_env, list.*, &head, list)) {
    return head;
  } else { return Error.FunctionClauseError; }
}

// slices
pub fn get_char_slice(erl_env: env, src_term: term) ![]u8 {
  var bin: binary = undefined;
  var res: []u8 = undefined;

  if (0 != e.enif_inspect_binary(erl_env, src_term, &bin)) {
    return bin.data[0..bin.size];
  } else { return Error.FunctionClauseError; }
}

// strings
pub fn get_c_string(erl_env: env, src_term: term) ![*c]u8 {
  var bin: binary = undefined;
  if (0 != e.enif_inspect_binary(erl_env, src_term, &bin)) {
    return bin.data;
  } else { return Error.FunctionClauseError;}
}

// atoms
pub const atom = e.ErlNifTerm;

pub fn make_atom(erl_env: env, atom_str: []const u8) term {
  return e.enif_make_atom_len(erl_env, @ptrCast([*c]const u8, &atom_str[0]), atom_str.len);
}

// pids
pub const pid = e.ErlNifPid;
pub fn get_pid(erl_env: env, src_term: term) !pid {
  var res :pid = undefined; 
  if (0 != e.enif_get_local_pid(erl_env, src_term, &res)) {
    return res;
  } else { return Error.FunctionClauseError; }
}

// implementation for :enomem

// create a global enomem string, then throw it.
const enomem_slice = "enomem"[0..];
pub fn enomem(erl_env: env) noreturn {
  var res = e.enif_raise_exception(erl_env, make_atom(erl_env, enomem_slice));
  unreachable;
}
// then throw it and return.
pub fn throw_enomem(erl_env: env) term {
  return e.enif_raise_exception(erl_env, make_atom(erl_env, enomem_slice));
}

// implementation for throw_function_clause_error;
const f_c_e_slice = "function_clause"[0..];
pub fn throw_function_clause_error(erl_env: env) term {
  return e.enif_raise_exception(erl_env, make_atom(erl_env, f_c_e_slice));
}
