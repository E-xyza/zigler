# Allocation using zigler

Zig the language has no offically supported allocator, and the standard library
datastructures are all allocator-agnostic.  

Zigler ships with three primary allocators, though you can certainly build
allocator strategies *on top* of those allocators.

## Raw Beam Allocator

The first allocator is `raw_beam_allocator`.  This allocator wraps the 
[nif allocator](https://www.erlang.org/doc/man/erl_nif.html#enif_alloc) 
provided by the BEAM.  You should use this allocator over `malloc` because it
often saves a syscall by using existing preallocated memory pools, because
it allows the VM to track how much memory your NIF is using, and possibly
gives better memory placement to avoid cache misses in your execution
thread.

```elixir
~Z"""
const beam = @import("beam");

pub fn allocate_raw(env: beam.env, count: usize) !beam.term {
    var slice = try beam.raw_beam_allocator.alloc(u16, count);
    defer beam.raw_beam_allocator.free(slice);

    for (slice) |*entry, index| {
        entry.* = @intCast(u16, index);
    }
    return beam.make(env, slice, .{});
}
"""

test "raw allocator" do
  assert [0, 1, 2] = allocate_raw(3)
end
```

> # raw allocator limitations {: .warning }
>
> because the raw allocator directly wraps the beam allocator, according to
> the documentation:
>
> > The returned pointer is suitably aligned for any built-in type that 
> > fit (sic) in the allocated memory.
>
> attempting to allocate memory aligned to a larger size (e.g. page-aligned 
> allocation) will fail using this allocator.

## Large Allocator

Zigler provides a `large allocator` which allows you to allocate memory ranges
that have a higher alignment than the maximum alignment for builtin types.
Note that using this allocator comes with a memory penalty.

```elixir
~Z"""
pub fn allocate_large_aligned(count: usize) !usize {
    const page = try beam.large_allocator.allocWithOptions(u8, count, 4096, null);
    defer beam.large_allocator.free(page);

    return @ptrToInt(page.ptr);
}
"""

test "aligned allocation" do
  assert 0 = rem(allocate_large_aligned(3), 4096)
end
```

## General Purpose Allocator

Zigler provides a version of the zig standard library's 
`GeneralPurposeAllocator` which is built on top of the large allocator.  Two 
advantages of using the general purpose allocator include optimized memory 
layouts for mixed allocation sizes and the ability to track memory leaks.

The state of the general purpose allocator is accessible using
`beam.allocator_.general_purpose_allocator_instance`

```elixir
~Z"""
pub fn leaks() !bool {
    const memory = try beam.general_purpose_allocator.alloc(u8, 8);
    defer beam.general_purpose_allocator.free(memory);

    // note that we haven't freed it yet, that happens after deferral.
    return beam.allocator_.general_purpose_allocator_instance.detectLeaks();
}

pub fn noleak() !bool {
    const memory = try beam.general_purpose_allocator.alloc(u8, 8);
    beam.general_purpose_allocator.free(memory);
    return beam.allocator_.general_purpose_allocator_instance.detectLeaks();
}
"""

test "leak checks with general purpose allocator" do
  assert leaks()
  refute noleak()
end
```