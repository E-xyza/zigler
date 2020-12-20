# Using Nifs

Nifs are the entrypoint between your BEAM code and your C ABI code.
Zigler provides semantics which are designed to make it easy to write
safe NIF code with the safety, memory guarantees that Zig provides.

## Basic operation

```elixir
~Z"""
/// nif: add_one/1
fn add_one(input: i32) i32 {
  return input + 1;
}
"""

test "add one" do
  assert 48 == add_one(47)
end
```

simply define a zig `fn` with desired input/output parameters.  Zigler will
generate a corresponding function in the surrounding Elixir module,
and a mismatched value will raise a `FunctionClauseError`.  The
following types are accepted as and outputs:

- `bool`
- `u8`
- `u16`
- `i32`
- `u32`
- `i64`
- `u64`
- `c_int` *
- `c_uint` *
- `c_long` *
- `c_ulong` *
- `isize`  *
- `usize`
- `beam.term`
- `beam.pid`

*only use these types to interface with C code and libraries.

Slices of all of the above, except `u8`, are also accepted (see below).

### Example: Slices

```elixir
~Z"""
/// nif: sum/1
fn sum(input: []f32) f32 {
  var total: f32 = 0.0;
  for (input) | value | { total += value; }
  return total;
}
"""

test "sum" do
  assert 6.0 == sum([1.0, 2.0, 3.0])
end
```

### Example: Binaries

Binaries are marshalled into `u8` slices.

```elixir
~Z"""
/// nif: capitalize/1
fn capitalize(input: []u8) []u8 {
  // note this is poorly bounds tested.
  input[0] = input[0] - 32;
  return input;
}
"""

test "capitalize" do
  assert "Foo" == capitalize("foo")
end
```

### Example: FunctionClauseError
```elixir
test "functionclauseerror" do
  assert_raise FunctionClauseError, fn ->
    capitalize(:atom)
  end
end
```

## Optional `beam.env` term

In order to build complex terms to be consumed by the rest of the BEAM, you
need to have access to the "environment" of the calling process.  In order
to be provided this, you may request it as the first parameter to your
zig function.  Play close attention to the arity of the nif declaration.


```elixir
~Z"""
/// nif: add_three/1
fn add_three(env: beam.env, number: i32) beam.term {
  return beam.make_ok_term(env, beam.make_i32(env, number + 3));
}
"""

test "add_three" do
  assert {:ok, 6} = add_three(3)
end
```

## Allocation

Use the [`beam.allocator`](beam.html#module-the-beam-allocator) to
allocate memory when you need it.  The allocator conforms to the Zig allocator
interface and importantly communicates allocations back to the BEAM for
tracking.

```elixir
~Z"""
// note: this function deliberately leaks memory!!

/// nif: allocate_leak/0
fn allocate_leak(env: beam.env) beam.term {
  _ = beam.allocator.alloc(u8, 10_000_000)
    catch return beam.raise_enomem(env);
  return beam.make_ok(env);
}
"""

test "allocate_leak" do
  start_memory = :erlang.memory[:total]
  assert :ok == allocate_leak()
  after_memory = :erlang.memory[:total]

  # evidence that we have leaked memory!
  assert 8_000_000 < (after_memory - start_memory)
end
```

## Yielding nifs

If you want to launch your nif as a yielding, use the `yielding` attribute.
This will allow the BEAM to schedule around the yield points.  This is the
most desirable way of creating a long-running NIF (when it gets repaired).

- during normal operation the `beam.yield()` function returns either a
  `beam.env` which updates the current environment for the benefit of the
  running nif.  If you need an environment for downstream puproses, use this
  value.
- if the running process is stopped by the VM, the nif will be caught by the
  cleanup method and will return `beam.YieldError`.  You **must** catch this
  error and return to allow the nif to be awaited.
- future editions of this feature will let you forward the yield-error to the
  cleanup method so you can use try instead of catch.

```elixir
~Z"""
/// nif: yielding/0 yielding
fn yielding(env: beam.env) beam.term {
  var count: i32 = 0;

  while (count < 10_000) {
    //
    // do some work here
    //

    // yield point
    _ = beam.yield() catch return beam.make_error(env);
    count += 1;
  }
  return beam.make_ok(env);
}
"""

test "yielding" do
  assert :ok = yielding()
end
```

## Threaded nifs

If you want to launch your nif as threaded, use the `threaded` attribute.  Note that
this a heavy handed operation that is inappropriate if you can do yielding or dirty
nifs.  You should use this strategy if:

- you are using 3rd party library code that can't take advantage of yielding nifs.
- you may need to run more nifs concurrently than you have dirty schedulers
- you don't mind the overhead of spinning up an OS thread.

```elixir
~Z"""
/// nif: threaded/0 threaded
fn threaded(env: beam.env) beam.term {
  std.time.sleep(10_000_000);

  return beam.make_ok(env);
}
"""

test "threaded" do
  assert :ok == threaded()
end
```

## Dirty nifs

If you want to launch your nif as a dirty, use the `dirty_cpu` or `dirty_io` attributes.
Note that by default, your vm will only have a limited number of dirty schedulers
available and launching the nif may fail if all of them are occupied.  You should use
this strategy if:

- you are using 3rd party library code that can't take advantage of yielding nifs.
- you don't mind that dirty_cpu jobs will be queued by the system if there the request
  rate exceeds availability.

```elixir
~Z"""
/// nif: dirty_cpu/0 dirty_cpu
fn dirty_cpu(env: beam.env) beam.term {
  std.time.sleep(10_000_000);

  return beam.make_ok(env);
}
"""

test "dirty_cpu" do
  assert :ok == dirty_cpu()
end
```
