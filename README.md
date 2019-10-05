# Zigler

**Zig Nifs made easy**

Wouldn't it be nice if you could make NIFs as easily as you can use the `asm`
keyword in C?

This is now possible, using the magic of Zig.

```elixir
defmodule ExampleZig do
  use Zigler, app: :my_app

  ~Z"""
  @nif("example_fun")
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

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `zigler` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:zigler, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/zigler](https://hexdocs.pm/zigler).

