# Zigler

Library test status:

- ![](https://github.com/ityonemo/zigler/workflows/Elixir%20CI%20-%20Linux/badge.svg)
- ![](https://github.com/ityonemo/zigler/workflows/Elixir%20CI%20-%20MacOS/badge.svg)
- ![](https://github.com/ityonemo/zigler/workflows/Elixir%20CI%20-%20Windows/badge.svg)
- ![](https://github.com/ityonemo/zigler/workflows/Elixir%20CI%20-%20FreeBSD/badge.svg)

## Installation: Elixir

### Obtaining Zig dependency

Run `mix zig.get` after `mix deps.get`.  This will provide a zig installation
in the directory pointed to by `:filename.basedir(:user_cache, "zigler")`.

You may also use a local installation of zig by specifying the environment variable
`ZIG_ARCHIVE_PATH`.

### Main Installation

Zigler is [available in Hex](https://hex.pm/packages/zigler), and the package can be installed
by adding `zigler` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:zigler, "~> 0.15.1", runtime: false}
  ]
end
```

## Installation: Erlang

### Zig dependency

TBD.

`~/.cache/zigler/zig-linux-<arch>-0.15.1`

### Main Installation

Erlang is only supported via rebar3.  You must enable the rebar_mix plugin and 
add zigler to your deps in rebar3.

Note that erlang support is highly experimental.  Please submit issues if you 
have difficulty.

```erlang
{plugins, [rebar_mix]}.

{deps, [{zigler, "0.14"}]}.

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
    return @intCast(string.len);
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
  const beam = @import("beam");

  pub fn double_atom(string: []u8) !beam.term {
    var double_string = try beam.allocator.alloc(u8, string.len * 2);
    defer beam.allocator.free(double_string);

    for (string, 0..) | char, i | {
      double_string[i] = char;
      double_string[i + string.len] = char;
    }

    return beam.make_into_atom(double_string, .{});
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
if {:unix, :linux} == :os.type() do
  defmodule Blas do
    use Zig,     
      otp_app: :zigler,
      c: [link_lib: {:system, "blas"}]
  
    ~Z"""
    const beam = @import("beam");
    const blas = @cImport({
        @cInclude("cblas.h");
    });

    const BadArgs = error { badarg };
  
    pub fn blas_axpy(a: f64, x: []f64, y: []f64) ![]f64 {
        if (x.len != y.len) return error.badarg;
    
        blas.cblas_daxpy(@intCast(x.len), a, x.ptr, 1, y.ptr, 1);
    
        return y;
    }
    """
  end
  
  test "we can use a blas shared library" do
    # returns aX+Y
    assert [11.0, 18.0] == Blas.blas_axpy(3.0, [2.0, 4.0], [5.0, 6.0])
  end
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

Zigler ships with a formatter.  To activate the formatter, adapt the following to your
`.formatter.exs`:

```elixir
[
  inputs: ~w[
    {mix,.formatter,.credo}.exs
    {config,lib,rel,test}/**/*.{ex,exs,zig}
    installer/**/*.{ex,exs}
  ],
  plugins: [Zig.Formatter]
]
```

## Erlang support (highly experimental)

Use of Zigler with erlang is possible using parse transforms.  You must obtain
zigler using the `rebar3` and the `rebar_mix` plugin.  Modules with zigler
nifs should inculde code into one or more `zig_code` attribute and pass 
zigler options (identical to the elixir options) into a `zig_opts` attribute.  
Zigler will then create appropriate functions matching the zig functions as
it does with elixir.  Please not that some features (such as integers > 64 
bits) are not currently supported in erlang, although nearly full feature parity
is planned.

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
