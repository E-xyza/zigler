const e = @cImport({
  @cInclude("<%= erl_nif_zig_h %>");
});
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

// create a global enomem string
const enomem_str = "enomem";
pub fn enomem(comptime T: type, env: ?*T.ErlNifEnv) T.ErlNifTerm {
  return T.enif_make_atom_len(env, @ptrCast([*c]const u8, &enomem_str[0]), 6);
}