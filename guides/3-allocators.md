# Allocation using zigler

Zig the language has no offically supported allocator, and the standard library
datastructures are all allocator-agnostic.  

Zigler ships with three primary allocators, though you can certainly build
allocator strategies *on top* of those allocators.

## Raw Beam Allocator

The first allocator is `raw_allocator`.  This allocator wraps the 
[nif allocator](https://www.erlang.org/doc/man/erl_nif.html#enif_alloc) 
provided by the BEAM.  You should use this allocator over `malloc` because it
often saves a syscall by using existing preallocated memory pools, because
it allows the VM to track how much memory your NIF is using, and possibly
gives better memory placement to avoid cache misses in your execution
thread.

```elixir
#~Z"""
#const beam = @import("beam");
#
#pub fn allocate_raw(env: beam.env, count: usize) !beam.term {
#    var slice = try beam.raw_allocator.alloc(u16, count);
#    defer beam.raw_allocator.free(slice);
#
#    for (slice, 0..) |*entry, index| {
#        entry.* = @intCast(index);
#    }
#    return beam.make(env, slice, .{});
#}
#"""
#
#test "raw allocator" do
#  assert [0, 1, 2] = allocate_raw(3)
#end
```

> ### raw allocator limitations {: .warning }
>
> because the raw allocator directly wraps the beam allocator, according to
> the documentation:
>
> The returned pointer is suitably aligned for any built-in type that 
> fit (sic) in the allocated memory.
>
> attempting to allocate memory aligned to a larger size (e.g. page-aligned 
> allocation) will fail using this allocator.

### Tracking memory.

> ### information in hidden globals {: .warning }
>
> Generally storing information in hidden globals is not a good idea.  Here
> it is done to illustrate the memory usage.  A better strategy would be to
> use [resources](5-resources.html)

```elixir
#~Z"""
#var global_zigler: []u8 = undefined;
#
#pub fn zigler_alloc() !void {
#    global_zigler = try beam.raw_allocator.alloc(u8, 1_000_000);
#}
#
#pub fn zigler_free() void {
#    beam.raw_allocator.free(global_zigler);
#}
#
#const c_stdlib = @cImport(@cInclude("stdlib.h"));
#
#var global_cstd: [*c]u8 = undefined;
#pub fn c_malloc() void {
#    global_cstd = @ptrCast(c_stdlib.malloc(1_000_000));
#}
#
#pub fn c_free() void {
#    c_stdlib.free(global_cstd);
#}
#"""
#
#test "zigler memory is tracked" do
#  Process.sleep(100)
#  start = :erlang.memory[:total]
#  zigler_alloc()
#  assert :erlang.memory[:total] - start >= 1_000_000
#  zigler_free()
#end
#
#test "malloc memory is not tracked" do
#  Process.sleep(100)
#  start = :erlang.memory[:total]
#  c_malloc()
#  assert :erlang.memory[:total] - start <= 1_000_000
#  c_free()
#end
#
```

## Large Allocator

Zigler provides a `large allocator` which allows you to allocate memory ranges
that have a higher alignment than the maximum alignment for builtin types.
Note that using this allocator comes with a memory penalty.

```elixir
#~Z"""
#pub fn allocate_large_aligned(count: usize) !usize {
#    const page = try beam.large_allocator.allocWithOptions(u8, count, 4096, null);
#    defer beam.large_allocator.free(page);
#
#    return @intFromPtr(page.ptr);
#}
#"""
#
#test "aligned allocation" do
#  assert 0 = rem(allocate_large_aligned(3), 4096)
#end
```

## General Purpose Allocator

Zigler provides a version of the zig standard library's 
`GeneralPurposeAllocator` which is built on top of the large allocator.  Two 
advantages of using the general purpose allocator include optimized memory 
layouts for mixed allocation sizes and the ability to track memory leaks.

The state of the general purpose allocator is accessible using
`beam.allocator_.general_purpose_allocator_instance`

```elixir
#~Z"""
#pub fn leaks() !bool {
#    const memory = try beam.general_purpose_allocator.alloc(u8, 8);
#    defer beam.general_purpose_allocator.free(memory);
#
#    // note that we haven't freed it yet, that happens on deferral,
#    // which lands after the return call.
#
#    return beam.allocator_.general_purpose_allocator_instance.detectLeaks();
#}
#
#pub fn noleak() !bool {
#    const memory = try beam.general_purpose_allocator.alloc(u8, 8);
#    beam.general_purpose_allocator.free(memory);
#    return beam.allocator_.general_purpose_allocator_instance.detectLeaks();
#}
#"""
#
#test "leak checks with general purpose allocator" do
#  assert leaks()
#  refute noleak()
#end
```

## beam.allocator

Zigler provides a `threadlocal` variable: `beam.allocator`.  This is set on entry
into the nif and defaults to `beam.raw_allocator`

```elixir
#~Z"""
#pub fn basic(env: beam.env) !beam.term {
#    const slice = try beam.allocator.alloc(u16, 4);
#    defer beam.allocator.free(slice);
#
#    for (slice, 0..) |*item, index| {
#        item.* = @intCast(index);
#    }
#
#    return beam.make(env, slice, .{});
#}
#"""
#
#test "leak checks with allocator" do
#  assert [0, 1, 2, 3] = basic()
#end
```

> ### Raw nifs {: .warning }
>
> For raw nifs, `beam.allocator` is not set, and may retain a value from an
> arbitrary previous nif invocation.  Consider usage of `beam.allocator` in a
> raw nif to be undefined unless it is set in the raw nif.

## Custom allocators

Because zigler's allocators conform to zig's allocator interface, you can use
any composed allocator in the standard library or any composable allocator 
from an imported zig package.

```elixir
#~Z"""
#const std = @import("std");
#
#pub fn with_arena(env: beam.env) !beam.term {
#    var arena = std.heap.ArenaAllocator.init(beam.allocator);
#    defer arena.deinit();
#
#    const allocator = arena.allocator();
#
#    const slice = try allocator.alloc(u16, 4);
#    defer allocator.free(slice);
#
#    for (slice, 0..) |*item, index| {
#        item.* = @intCast(index);
#    }
#
#    return beam.make(env, slice, .{});
#}
#"""
#
#test "arena allocator" do
#  assert [0, 1, 2, 3] == with_arena()
#end
```

