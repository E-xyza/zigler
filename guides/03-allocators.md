# Allocations

Zig the language has no offically supported allocator, and the standard library datastructures are
all allocator-agnostic. 

Zigler ships with three primary allocators, though you can certainly build allocator strategies *on
top* of those allocators.

## Basic Allocator

The first allocator is `allocator`. This allocator wraps the [nif
allocator](https://www.erlang.org/doc/man/erl_nif.html#enif_alloc) provided by the BEAM in the zig
allocator interface. You should generally use this allocator over `malloc` because it often saves a
syscall by using existing preallocated memory pools, because it allows the VM to track how much
memory your NIF is using, and possibly gives better memory placement to avoid cache misses in your
execution thread.

```elixir
~Z"""
const beam = @import("beam");

pub fn allocate_raw(count: usize) !beam.term {
    const slice = try beam.allocator.alloc(u16, count);
    defer beam.allocator.free(slice);

    for (slice, 0..) |*entry, index| {
        entry.* = @intCast(index);
    }
    return beam.make(slice, .{});
}
"""

test "raw allocator" do
  assert [0, 1, 2] = allocate_raw(3)
end
```

> ### allocator limitations {: .warning}
>
> because the basic allocator directly wraps the beam allocator, according to the documentation:
>
> The returned pointer is suitably aligned for any built-in type that fit (sic) in the allocated
> memory.
>
> attempting to allocate memory aligned to a larger size (e.g. page-aligned allocation) will fail
> using this allocator.

### Tracking memory.

> ### information in hidden globals {: .warning}
>
> Generally storing information in hidden globals is not a good idea. Here it is done to illustrate
> the memory usage. A better strategy would be to use [resources](5-resources.html)

```elixir
~Z"""
var global_zigler: []u8 = undefined;

pub fn zigler_alloc() !void {
    global_zigler = try beam.allocator.alloc(u8, 1_000_000);
}

pub fn zigler_free() void {
    beam.allocator.free(global_zigler);
}

const c_stdlib = @cImport(@cInclude("stdlib.h"));

var global_cstd: [*c]u8 = undefined;
pub fn c_malloc() void {
    global_cstd = @ptrCast(c_stdlib.malloc(1_000_000));
}

pub fn c_free() void {
    c_stdlib.free(global_cstd);
}
"""

test "zigler memory is tracked" do
  Process.sleep(100)
  start = :erlang.memory[:total]
  zigler_alloc()
  assert :erlang.memory[:total] - start >= 1_000_000
  zigler_free()
end

test "malloc memory is not tracked" do
  Process.sleep(100)
  start = :erlang.memory[:total]
  c_malloc()
  assert :erlang.memory[:total] - start <= 1_000_000
  c_free()
end
```

## Wide Alignment Allocator

Zigler provides a `wide_alignment_allocator` which allows you to allocate memory ranges that have a
higher alignment than the maximum alignment for builtin types. 

> ### memory penalty {: .warning}
>
> Note that using this allocator comes with a memory penalty, so use as a general allocator is not
> recommended.

```elixir
~Z"""
pub fn allocate_large_aligned(count: usize) !usize {
    const page = try beam.allocator.allocWithOptions(u8, count, 4096, null);
    defer beam.allocator.free(page);

    return @intFromPtr(page.ptr);
}
"""

test "aligned allocation" do
  assert 0 = rem(allocate_large_aligned(3), 4096)
end
```

## Other Allocators Provided

### DebugAllocator

Zigler provides a version of the zig standard library's `DebugAllocator` which is built on top of
the large allocator. You may use this to easily track memory leaks.

The state of the global general purpose allocator is accessible using `beam.allocator_.debug_allocator_instance`

You may also create a custom general purpose allocator instance using
`beam.make_debug_allocator_instance`, whcih is what happens on a per-nif basis if the nif is
checking for leaks.

```elixir
~Z"""
pub fn leaks() !bool {
    const memory = try beam.debug_allocator.alloc(u8, 8);
    defer beam.debug_allocator.free(memory);

    // note that we haven't freed it yet, that happens on deferral,
    // which lands after the return call.

    return beam.allocator_.debug_allocator_instance.detectLeaks();
}

pub fn noleak() !bool {
    const memory = try beam.debug_allocator.alloc(u8, 8);
    beam.debug_allocator.free(memory);
    return beam.allocator_.debug_allocator_instance.detectLeaks();
}
"""

test "leak checks with debug allocator" do
  require Logger
  Logger.warning("====== the following leak message is expected: =========== START")
  Process.sleep(200)
  assert leaks()
  Logger.warning("=========================================================== END")

  refute noleak()
end
```

## Building composable allocators backed by zig's beam allocator

Because zigler's beam allocators conform to zig's allocator interface, you may use use any
composable allocator in the standard library or any composable allocator from an imported zig
package, passing any one of the beam allocators into place. 

```elixir
~Z"""
pub fn with_arena() !beam.term {
    const std = @import("std");

    var arena = std.heap.ArenaAllocator.init(beam.allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const slice = try allocator.alloc(u16, 4);
    defer allocator.free(slice);

    for (slice, 0..) |*item, index| {
        item.* = @intCast(index);
    }

    return beam.make(slice, .{});
}
"""

test "arena allocator" do
  assert [0, 1, 2, 3] == with_arena()
end
```

## Zig allocators in `beam.get`

You may use zig standard library allocators or other custom alloctors in the `beam.get` functions to
instantiate data where it's the allocator's responsibility to free it at the end.

```elixir
~Z"""
pub fn arena_sum(array: beam.term) !u64 {
    // use the zig std-provided heap allocator.
    const malloc = @import("std").heap.c_allocator;

    const slice = try beam.get([]u64, array, .{.allocator = malloc});
    defer malloc.free(slice);

    var total: u64 = 0;

    for (slice) |item| { total += item; }

    return total;
}
"""

test "arena sum" do
  assert 6 == arena_sum([1, 2, 3])
end
```
