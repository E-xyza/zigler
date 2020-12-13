# Using Nifs

Nifs are the entrypoint between your BEAM code and your C ABI code.
Zigler provides semantics which are designed to make it easy to write
safe

## Basic operation

```zig
/// nif: my_nif/1
fn my_nif(input: i32) i32 {
  return input + 1;
}
```

simply define a zig `fn` with desired input parameters.  Zigler will
generate a corresponding function in the surrounding Elixir module,
and a mismatched value will raise a `FunctionClauseError`.  The
following types are accepted as inputs:

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
- `beam.atom`
- `beam.pid`

*only use these types to interface with C code and libraries.

Slices of all of the above, except `u8`, are also accepted (see below).

## Example (with slices)

```zig
/// nif: sum/1
fn sum(input: []f32) f32 {
  var total: f32 = 0.0;
  for (input) | value | { total += value };
  return total;
}
```

## Binaries

Binaries are marshalled into `u8` slices.

```zig
/// nif: double/1
fn capitalize(input: []u8) []u8 {
  // note this is poorly bounds tested.
  input[0] = input[0] - 32
  return input;
}
```

## Allocation

Use the [`beam.allocator`](beam.html#module-the-beam-allocator) to
allocate memory when you need it.

## Optional `beam.env` term

In order to build terms to be consumed by the rest of the BEAM, you need
to have access to the "environment" of the calling process.  In order
to be provided this, you may request it as the first parameter to your
zig function.  Play close attention to the arity of the nif declaration.

```zig
/// nif: add_3/1
fn add_3(env: beam.env, number: i32) beam.term {
  return beam.make_i32(env, number + 3);
}
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

```zig
/// nif: yielded_nif/1 yielding
fn threaded_nif(env: beam.env, input: u64) u64 {
  ...
  // code that takes a long time
  ...

  // yield point
  _ = beam.yield() catch return 0;

  return my_result;
}
```

## Threaded nifs

If you want to launch your nif as threaded, use the `threaded` attribute.  Note that
this a heavy handed operation that is inappropriate if you can do yielding or dirty
nifs.  You should use this strategy if:

- you are using 3rd party library code that can't take advantage of yielding nifs.
- you need to run more nifs concurrently than you have dirty schedulers

```zig
/// nif: threaded_nif/1 threaded
fn threaded_nif(env: beam.env, input: u64) u64 {
  ...
  // code that takes a long time
  ...
  return my_result;
}
```

## Dirty nifs

If you want to launch your nif as a dirty, use the `dirty_cpu` or `dirty_io` attributes.
Note that by default, your vm will only have a limited number of dirty schedulers
available and launching the nif may fail if all of them are occupied.  You should use
this strategy if:

- you are using 3rd party library code that can't take advantage of yielding nifs.
- you can throttle requests to limit how many are using the available schedulers.

```zig
/// nif: dirty_nif/1 dirty_cpu
fn dirty_nif(env: beam.env, input: u64) u64 {
  ...
  // code that takes a long time
  ...
  return my_result;
}
```
