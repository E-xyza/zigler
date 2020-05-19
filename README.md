# Zigler

Library test status:
![](https://github.com/ityonemo/zigler/workflows/Elixir%20CI/badge.svg)

Dependent package test status:
![](https://github.com/ityonemo/zigler_test/workflows/Elixir%20CI/badge.svg)

## Installation

Zigler is [available in Hex](https://hex.pm/zigler), and the package can be installed
by adding `zigler` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:zigler, "~> 0.3.0", runtime: false}
  ]
end
```

## Documentation

Docs can be found at [https://hexdocs.pm/zigler](https://hexdocs.pm/zigler).

once you have this dependency, you should cache the zig build tools by running the following:

`mix zigler.get_zig latest`

## Currently supported platforms

- Linux
- FreeBSD (tested, but not subjected to CI)
- MacOS (I believe it works but is still offically untested)

## Zig Nifs made easy

Wouldn't it be nice if you could make NIFs as easily as you can use the `asm`
keyword in C?

This is now possible, using the magic of Zig.

```elixir
defmodule ExampleZig do
  use Zigler, otp_app: :my_app

  ~Z"""
  /// nif: example_fun/2
  fn example_fun(value1: f64, value2: f64) bool {
    return value1 > value2;
  }
  """

end

iex> ExampleZig.example_fun(0.1, 0.4)
false

iex> ExampleZig.example_fun(0.8, -0.8)
true
```

Zigler will do automatic type marshalling between Elixir code and Zig code.
It will also convert trickier types into types you care about, for example:

```elixir
defmodule ZigCollections do
  use Zigler, otp_app: :my_app
  ~Z"""
  /// nif: string_count/1
  fn string_count(string: []u8) i64 {
    return @intCast(i64, string.len);
  }

  /// nif: list_sum/1
  fn list_sum(array: []f64) f64 {
    var sum: f64 = 0.0;
    for(array) | item | {
      sum += item;
    }
    return sum;
  }
  """
end

iex> ZigCollections.string_count("hello zig")
9
iex> ZigCollections.list_sum([1.0, 2.0, 3.0])
6.0
```

Memory allocation with zigler is easy!  A standard BEAM allocator is provided for you,
so any zig code you import will play nice with the BEAM.

```elixir
defmodule Allocations do
  use Zigler, otp_app: :my_app
  ~Z"""
  /// nif: double_atom/1
  fn double_atom(env: beam.env, string: []u8) beam.atom {
    var double_string = beam.allocator.alloc(u8, string.len * 2) catch {
      return beam.raise_enomem(env);
    };

    defer beam.allocator.free(double_string);

    for (string) | char, i | {
      double_string[i] = char;
      double_string[i + string.len] = char;
    }

    return beam.make_atom(env, double_string);
  }
  """
end

iex> Allocations.double_atom("foo")
:foofoo

```

It is a goal for Zigler to make using *it* to bind C libraries easier
than using C to bind C libraries.  Here is an example:

```elixir
defmodule BlasDynamic do
  use Zigler,
    otp_app: :zigler,
    libs: ["/usr/lib/x86_64-linux-gnu/blas/libblas.so"],
    include: ["/usr/include/x86_64-linux-gnu"]

  ~Z"""
  const blas = @cImport({
    @cInclude("cblas.h");
  });

  /// nif: blas_axpy/3
  fn blas_axpy(env: beam.env, a: f64, x: []f64, y: []f64) beam.term {
    if (x.len != y.len) {
      return beam.raise_function_clause_error(env);
    }

    blas.cblas_daxpy(@intCast(c_int, x.len), a, x.ptr, 1, y.ptr, 1);

    return beam.make_f64_list(env, y) catch {
      return beam.raise_function_clause_error(env);
    };
  }
  """
end

test "we can use dynamically-linked blas" do
  # returns aX+Y
  assert [11.0, 18.0] == BlasDynamic.blas_axpy(3.0, [2.0, 4.0], [5.0, 6.0])
end
```

Zigler even has support for zig docstrings.

```elixir

defmodule AllTheDocs do
  use Zigler, otp_app: :zigler
  ~Z"""
  /// a zero-arity function which returns 47.
  /// nif: zero_arity/0
  fn zeroarity() i64 {
    return 47;
  }
  """
end

iex> h AllTheDocs.zeroarity

                                def zeroarity()

a zero-arity function which returns 47.
```

## Zigler Principles

1. Make doing the right thing easy.
2. Use magic, but sparingly.
3. Let the user see behind the curtain.
4. Let the user dial in magic as they choose.
5. Magic shouldn't get in the way
