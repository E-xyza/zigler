# Using Nifs

Nifs are the entrypoint between your BEAM code and your C ABI code.  Zigler 
provides semantics which are designed to make it easy to write safe NIF code 
with the safety, memory guarantees that Zig provides.

## Preamble

Near the top of your module you should use the `use Zig` directive.  This will 
activate Zigler to seek zig code blocks and convert them to functions.  You 
must provide the `otp_app` option, which enables Zigler to find a directory to 
place compilation artifacts (such as libraries) so that they can be shipped 
with releases.  By default, this will be `/priv/lib`.

> ### compilation artifacts {: .warning }
>
> defaults for compilation artifacts may change in future versions of 
> zigler.

Note that we can ship zig code in *any* module in-place, so they will live 
alongside other functions or even other macro alterations you make to the 
module.  In this case, we'll build our code into a module that will be tested 
with ExUnit.

```elixir
defmodule NifGuideTest do
  use Zig, otp_app: :zigler
  use ExUnit.Case, async: true
```

## Basic function writing

Once zigler has been activated for a module, write `~Z` code anywhere and this 
code will be assembled into a zig file that will be compiled into the nif 
artifact.  

Then write your desired zig function Zigler will also mount functions with the 
*same name* as the function in the body of the module.

### Example: simple scalar values

```elixir
~Z"""
pub fn add_one(input: i32) i32 {
  return input + 1;
}
"""

test "add one" do
  assert 48 == add_one(47)
end
```

Note that Zigler will automatically marshal input and output values across the 
nif boundary.  The following scalar types are accepted by zigler:

- signed integer (`i0`..`i65535`), including non-power-of-two values
- unsigned integer (`u0`..`u65535`), including non-power-of-two values
- `usize`, `isize`, architecture-dependent size (roughly `size_t` and 
  `ssize_t` in C)
- `c_char`, `c_short`, `c_ushort`, `c_int`, `c_uint`, `c_ulong`, 
  `c_longlong`, `c_ulonglong`, which are architecture-dependent
  integer sizes mostly used for c interop.
- floats `f16`, `f32`, and `f64`.
- `bool` (use the atoms `true` or `false` exclusively)

> #### Floating point datatypes {: .info }
>
> Floating point datatypes can take the atoms `:infinity`, 
> `:neg_infinity`, and `:NaN`.

> #### Boolean datatypes {: .info }
>
> You must pass boolean datatypes `true` or `false` atoms.

Zigler can also marshal list parameters into array, and array-like 
datatypes:

### Example: Array-like datatypes

```elixir
~Z"""
pub fn sum(input: []f32) f32 {
  var total: f32 = 0.0;
  for (input) | value | { total += value; }
  return total;
}
"""

test "sum" do
  assert 6.0 == sum([1.0, 2.0, 3.0])
end
```

The following array-like datatypes are allowed for parameters:
- arrays `[3]T` (for example).  Note the length is compile-time known.
- slices `[]T`
- pointers to arrays `*[3]T` (for example).
- multipointers `[*]T`
- sentinel-terminated versions of all of the above.
- cpointers `[*c]T`

### Example: Array-like datatypes as binaries

For all scalar child types, Array-like datatypes may be passed as binaries,
thus the following code works with no alteration:

```elixir
test "sum, with binary input" do
  assert 6.0 == sum(<<1.0 :: float-size(32)-native, 2.0 :: float-size(32)-native, 3.0 :: float-size(32)-native>>)
end
```

This also results in a natural interface for treating BEAM binaries as `u8` 
arrays or slices.

### Example: Marshalling errors

Zigler will generate code that protects you from sending incompatible 
datatypes to the desired function:

```elixir
test "marshalling error" do
  assert_raise ArgumentError, """
  errors were found at the given arguments:

    * 1st argument: 
  
       expected: <<_::_ * 32>> | list(float | :infinity | :neg_infinity | :NaN) (for `[]f32`)
       got: `:atom`
  """, fn ->
    sum(:atom)
  end
end
```

For more on marshalling collection datatypes, see [`collections`](collections.html).

## Marshalling types manually

You may also manually marshal types into and out of the beam by using the
[`beam.term`](beam.html#term) datatype.  To do so, you must first import 
the [`beam`](beam.html) package.  The `beam.term` type is an opaque, wrapped 
datatype that ensures safe manipulation of terms as a token in your zig code.

You will also need [`beam.env`](beam.html#env) environment variable to reference
the execution environmet of your nif, to box and unbox data from the beam terms.

```elixir
~Z"""
const beam = @import("beam");

pub fn manual_addone(value_term: beam.term) !beam.term {
    const value = try beam.get(i32, value_term, .{});
    return beam.make(value + 1, .{});
}
"""

test "manual marshalling" do
  assert 48 == manual_addone(47)
end
```

### A few notes on the above code.

- to marshal a value out of a `beam.term` and into a zig static type,
  use [`beam.get`](beam.html#get).  This is a failable function, and in
  this case we hoist the failure in the function return.
- to marshal a value into a `beam.term` from a zig static type, use
  [`beam.make`](beam.html#make).  This function *does not fail*.
- for more information on the final options parameter of `get` and
  `make`, see their respective documentation.
- zigler will translate the hoisted marshalling failures into BEAM 
  exceptions.
- zigler automatically ignores the `beam.env` parameter in the first
  position, and assigns the correct arity (1).  Compilation will
  fail if a `beam.env` parameter is present in any other location.
