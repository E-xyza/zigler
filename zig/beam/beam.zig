const e = @import("erl_nif.zig").c;
const std = @import("std");

const Allocator = std.mem.Allocator;

// add syntactic sugar!
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

// env
pub const env = ?*e.ErlNifEnv;

// terms
pub const term = e.ErlNifTerm;

// atoms
pub const atom = e.ErlNifTerm;
pub fn make_atom(erl_env: env, atom_str: []const u8) term {
  return e.enif_make_atom_len(erl_env, @ptrCast([*c]const u8, &atom_str[0]), atom_str.len);
}

// implementation for :enomem

// create a global enomem string, then throw it.
const enomem_slice = "enomem"[0..];
pub fn enomem(erl_env: env) noreturn {
  var res = e.enif_raise_exception(erl_env, make_atom(erl_env, enomem_slice));
  unreachable;
}
