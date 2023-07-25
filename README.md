# Zigler

Library test status:
![](https://github.com/ityonemo/zigler/workflows/Elixir%20CI/badge.svg)

## Installation: Elixir

Zigler is [available in Hex](https://hex.pm/packages/zigler), and the package can be installed
by adding `zigler` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:zigler, "~> 0.10.1", runtime: false}
  ]
end
```

Then you should run `mix zig.get` to download Zig 0.10.1

## Installation: Erlang

Erlang is only supported via rebar3.  You must enable the rebar_mix plugin and 
add zigler to your deps in rebar3.

Note that erlang support is highly experimental.  Please submit issues if you 
have difficulty.

```erlang
{plugins, [rebar_mix]}.

{deps, [{zigler, "0.10"}]}.

```

## Documentation

Docs can be found at [https://hexdocs.pm/zigler](https://hexdocs.pm/zigler).

## Currently supported platforms

- Linux
- FreeBSD (tested, but not subjected to CI)
- MacOS

- Nerves cross-compilation is supported out of the box.

## Zig Nifs made easy

Wouldn't it be nice if you could make NIFs as easily as you can use the `asm`
keyword in C?

This is now possible, using the magic of Zig.

```elixir
defmodule ExampleZig do
  use Zig, otp_app: :zigler
  ~Z"""
  pub fn example_fun(value1: f64, value2: f64) bool {
    return value1 > value2;
  }
  """
end

test "example nifs" do
  assert ExampleZig.example_fun(0.8, -0.8)
  refute ExampleZig.example_fun(0.1, 0.4)
end
```

Zigler will do automatic type marshalling between Elixir code and Zig code.
It will also convert trickier types into types you care about, for example:

```elixir
defmodule ZigCollections do
  use Zig, otp_app: :zigler
  ~Z"""
  pub fn string_count(string: []u8) i64 {
    return @intCast(i64, string.len);
  }

  pub fn list_sum(array: []f64) f64 {
    var sum: f64 = 0.0;
    for(array) | item | {
      sum += item;
    }
    return sum;
  }
  """
end

test "type marshalling" do
  assert 9 == ZigCollections.string_count("hello zig")
  assert 6.0 == ZigCollections.list_sum([1.0, 2.0, 3.0])
end
```

Memory allocation with zigler is easy!  A standard BEAM allocator is provided for you,
so any zig code you import will play nice with the BEAM.

```elixir
defmodule Allocations do
  use Zig, otp_app: :zigler
  ~Z"""
  pub fn double_atom(env: beam.env, string: []u8) beam.term {
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

test "allocations" do
  assert :foofoo == Allocations.double_atom("foo")
end
```

It is a goal for Zigler to make using *it* to bind C libraries easier
than using C to bind C libraries.  Here is an example:

```elixir
defmodule Blas do
  use Zig,     
    otp_app: :zigler
    link_lib: {:system, "blas"},

  ~Z"""
  const beam = @import("beam");
  const blas = @cImport({
    @cInclude("cblas.h");
  });

  pub fn blas_axpy(env: beam.env, a: f64, x: []f64, y: []f64) beam.term {
    if (x.len != y.len) {
      return beam.raise_function_clause_error(env);
    }

    blas.cblas_daxpy(@intCast(c_int, x.len), a, x.ptr, 1, y.ptr, 1);

    return y;
  }
  """
end

test "we can use a blas shared library" do
  # returns aX+Y
  assert [11.0, 18.0] == Blas.blas_axpy(3.0, [2.0, 4.0], [5.0, 6.0])
end
```

### Documentation (Elixir-only)

You can document nif functions, local functions, zig structs, variables, and types.
If you document a nif function, it will be a part of the module documentation, and
accessible using the iex `h` method, etc.

Example:

```elixir
defmodule Documentation do
  use Zig, otp_app: :zigler
  ~Z"""
  /// a zero-arity function which returns 47.
  pub fn zero_arity() i64 {
    return 47;
  }
  """
end
```

### Formatting (Elixir-only)

A mix format plugin is available through the `zigler_format` package.
[See the installation instructions](https://github.com/v0idpwn/zigler_format#installation)

## Erlang support

Use of Zigler with erlang is possible using parse transforms.  Annotate the zig
code into a `zig_code` attribute and pass zigler options (identical to the elixir
options) into a `zig_opts` attribute.  Zigler will then create appropriate
functions matching the zig functions.

```erlang
-module(erlang_zigler_module).
-compile({parse_transform, zigler}). 
-export([foo/1, foo/0]).

-zig_code("
pub fn foo() i32 {
    return 47;
}
").

-zig_opts([{otp_app, zigler}]).

foo(X) ->
    47 + X.
```

## Zigler Principles

1. Make being a good citizen of the BEAM easy.
2. Use magic, but sparingly, only to prevent errors.
3. Let the user see behind the curtain.
4. Let the user opt out of magic.
5. Magic shouldn't get in the way.
