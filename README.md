# Zigler

## Zig Nifs made easy

Wouldn't it be nice if you could make NIFs as easily as you can use the `asm`
keyword in C?

This is now possible, using the magic of Zig.

```elixir
defmodule ExampleZig do
  use Zigler, app: :my_app

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
  use Zigler, app: :my_app
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
  use Zigler, app: :my_app
  ~Z"""
  /// nif: double_atom/1
  fn double_atom(env: beam.env, string: []u8) beam.atom {
    var double_string = beam.allocator.alloc(u8, string.len * 2)
      catch beam.enomem(env);

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

Zigler even has support for zig docstrings.

```elixir

defmodule AllTheDocs do
  use Zigler, app: :zigler
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

## Installation

```elixir
def deps do
  [
    {:zigler, git: "https://github.com/ityonemo/zigler.git"}
  ]
end
```

once you have this dependency, you should cache the zig build tools by running the following:

`mix zigler.get_zig latest`

## Future installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `zigler` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:zigler, "~> 0.1.0"}
  ]
end
```

You'll also want to grab the latest zig binary and cache it with zigler.
You can do this by executing:

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/zigler](https://hexdocs.pm/zigler).
