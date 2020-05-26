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

## Dirty nifs

If you want to launch your nif as a dirty, use the `dirty_cpu` or
`dirty_io` attributes.  Note that by default, your vm will only have
a limited number of dirty threads available and launching the nif
may fail if all of them are occupied.

```zig
/// nif: dirty_nif/1 dirty_cpu
fn dirty_nif(env: beam.env, input: u64) u64 {
  ...
  // code that takes a long time
  ...
  return my_result;
}
```

## Future feature:  `long`

An upcoming feature will be the ability to run a function inside of a
sidecar (OS) process.  There is a considerable amount of boilerplate
required to safely wrap launching such a process, and Zigler will take
care of that for you.

### Example

```zig
/// nif: my_nif/1 long
fn my_nif(env: beam.env, input: beam.term) u64 {
  ...
  // code that takes a long time
  ...
  return my_result;
}
```

In the above example situation, the result `my_result` will be returned
to the process which called it, which will block awaiting a completion
message, without consuming NIF timeslices except for entry into the
function (serialization of the input data).

### Optional parent pid example

In some instances, you may want to monitor the parent pid from within the
long-running NIF. In those situations, if you request the parent's pid as
the second term you can for example, effectively monitor to silently quit
out of loops in case the parent dies.

If you don't want the parent pid to block awaiting a response, use the
`long:detached` attribute.

```zig
/// nif: my_nif/1 long:detached
fn my_nif(env: beam.env, parent: beam.pid, input: beam.term) void {
  while (true) {
    ...
    // code inside of a loop
    ...
    if is_dead(env, parent) break;
  }
}
```

This attribute may be combined with the `managed` or `dirty` attributes.
You should use `dirty long` nifs if you think serialization of your BEAM
data structure will take a very long time.

## Future feature:  `managed` and `managed:arena`

An upcoming feature will be the ability to use a 'managed' allocation
system.  Within a managed nif, performing `free` operations on allocated data
is optional, preventing the risk of memory leaks.  All memory fetched from the
managed allocator will be automatically freed on function exit.  It is
important to never pass these data into a `resource`.  If you don't want to
ever perform free operations you can use `managed:arena` which will use an
arena allocation strategy, which can potentially offer better latency.

### Example

```zig
/// nif: my_nif/1 managed
fn my_nif(env: beam.env, input: beam.term) beam.term {
  beam.managed.alloc(u8, 100)
  ...
}
```

This attribute may be combined with the `long` attribute.
