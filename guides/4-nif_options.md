# Nif options

Zigler gives you several ways to configure compilation options for nifs.  These
options are part of the `use Zig` directive, in the keyword options under the 
key `:nifs`.  This key itself points to a keyword list where the keys are the
names of the nif functions and the values are a keyword list of options that
apply to that function.

This guide shows you all of the nif options except for options related to 
[Resources](5-resources.html), [C integration](6-c_integration.html), or
[Concurrency](7-concurrency.html).

## Automatic options (elixir)

To declare that functions should have their options automatically determined,
use `...` in the nifs parameter list.  Nifs which sholud have manually decided
options should come after the `...` as a keyword list.  If all options are
automatically determined, then omitting the `:nif` keyword completely is valid.

```elixir
defmodule AutomaticOptions do
  use Zig, 
    otp_app: :zigler,
    nifs: [...]

  ~Z"""
  pub fn noop() void {}
  """
end
```

## Automatic options (erlang)

Erlang cannot interpret the `...` AST in elixir, so you must use `[auto]`
atom in the nifs options instead.

```erlang
-zig_opts([{nifs, [auto]}])
```

## Raw calls

Raw calls can be signified with the `raw` option; this option takes the arity
of the function as the value associated with the key.

> ### multiple arities {: .info }
>
> Currently, multiple arities are not supported, but multiple arities will be
> supported in a future release.

The normal header for a BEAM nif is as follows:

`static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])`

In order to support usage of BEAM nifs in the most flexible fashion, you may
write your nifs as `raw` nifs, which looks as follows:

```elixir
defmodule RawCallTest do
  use ExUnit.Case, async: true

  use Zig, 
    otp_app: :zigler,
    nifs: [
        raw_call_beam: [raw: 1],
        raw_call_erl_nif: [raw: 1]
    ]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn raw_call_beam(count: c_int, list: [*]const beam.term) beam.term {
      return beam.make(.{.count = count, .item = list[0]}, .{});
  }

  pub fn raw_call_erl_nif(count: c_int, list: [*]const e.ErlNifTerm) e.ErlNifTerm {
      return beam.make(.{.count = count, .item = beam.term{.v = list[0]}}, .{}).v;
  }
  """

  test "raw call with beam package" do
    assert %{count: 1, item: {:foo, "bar"}} = raw_call_beam({:foo, "bar"})
  end

  test "raw call with erl_nif package" do
    assert %{count: 1, item: {:foo, "bar"}} = raw_call_erl_nif({:foo, "bar"})
  end
end
```

Note that either the `beam.term` or the `e.ErlNifTerm` forms can be used.

## Return type

In [Collections](2-collections.html) we saw how certain collection types could 
be manually marshalled into alternative representations using 
[`beam.make`](beam.html#make).  This can be handled as a nif configuration as 
follows.  The advantage to doing it this way is that the typespec for the
function will correctly reflect the return type.

```elixir
defmodule ReturnTypeTest do
  use ExUnit.Case, async: true

  use Zig, 
    otp_app: :zigler,
    nifs: [
        returns_binary: [return: :binary],
        returns_charlist: [return: :list]
    ]

  ~Z"""
  pub fn returns_binary() [3]u16 {
      return [3]u16{47, 48, 49};
  }

  pub fn returns_charlist() []const u8 {
      return "Hello world!";
  }
  """

  test "returns binary" do
    assert <<47, 0, 48, 0, 49, 0>> = returns_binary()
  end

  test "returns charlist" do
    assert ~C'Hello world!' = returns_charlist()
  end
end
```

## Alias

It's possible to create a new function which is an alias of another function.
This is how it's done:

```elixir
defmodule AliasTest do
  use ExUnit.Case, async: true

  use Zig, 
    otp_app: :zigler,
    nifs: [
      ...,
      new_function: [alias: :old_function]
    ]

  ~Z"""
  pub fn old_function() u32 {
      return 47;
  }
  """

  test "both main and alias functions work" do
    assert 47 == old_function()
    assert 47 == new_function()
  end
end
```

## Args options

Arguments can also take options, using `args: [...]`

## Noclean

If you want to disable automatic allocator cleanup of datatypes, you can do so
either in the `:return` or `:args` section by including the `:noclean` option.  
This is most useful if your args or return data are going to be persisted 
beyond the lifetime of the nif call, though doing this in many cases is not 
recommended.

This flag can be stacked with previous options, for example:  
`return: [:noclean, :binary]`.

## Leak Check

It's possible to wrap each function call in its own instance of 
[`beam.general_purpose_allocator`](beam.html#general_purpose_allocator)
bound into the [`beam.allocator`](beam.html#allocator) threadlocal variable.
If you tag your nif as `leak_check`, it will check that `beam.allocator` has
cleared all of its contents at the end of the function call, and if that hasn't 
happened, it raises.

> ## leak check warning {: .warning }
>
> leak check doesn't seem to be working in 0.11.0 and will return in 0.11.1

```elixir
defmodule LeakCheckTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [check_me: [leak_check: true]]

  ~Z"""
  const beam = @import("beam");
  pub fn check_me() !void {
      _ = try beam.allocator.create(u8);
  }
  """

  @tag :skip
  test "leak check" do
    assert_raise RuntimeError, "memory leak detected in function `check_me/0`", fn ->
      check_me()
    end
  end
end
```

`leak_check` can also be applied to all nifs in the module:

```elixir
defmodule LeakCheckAllTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    leak_check: true

  ~Z"""
  const beam = @import("beam");
  pub fn check_me() !void {
      _ = try beam.allocator.create(u8);
  }
  """

  @tag :skip
  test "leak check" do
    assert_raise RuntimeError, "memory leak detected in function `check_me/0`", fn ->
      check_me()
    end
  end
end
```

> ### conditional leak checks {: .warning }
>
> It's not currently possible to make conditional leak checks, but this will be
> fixed in the future.

## Typespec override (Elixir-only)

Typespecs can be overridden by using the `spec` option, this syntax should 
match that of a normal typespec.  Note that overriding the autogenerated 
typespec is the only way to specify the type of function which uses 
[`beam.term`](beam.html#term) in its arguments or return.

For example:

```elixir
defmodule Override do
  use Zig, 
    otp_app: :zigler,
    nifs: [typespec_override: [spec: (integer -> integer)]]

  ~Z"""
  const beam = @import("beam");
  pub fn typespec_override(term: beam.term) !beam.term {
      const input = try beam.get(u32, term, .{});
      return beam.make(input + 1, .{});
  }
  """
end
```

## Disable documentation

Documentation can be disabled with the `docs: false` option.

```elixir
defmodule DisableDoc do
  use Zig, 
    otp_app: :zigler,
    nifs: [nodocs: [docs: false]]

  ~Z"""
  pub fn nodocs() void {}
  """
end
```

## Ignore

Public functions can be ignored and *not* converted into nifs by filling out 
the `:ignore` option in `use Zig` directive.

```elixir
defmodule IgnoreTest do
  use ExUnit.Case, async: true

  use Zig, 
    otp_app: :zigler,
    ignore: [:ignored]

  ~Z"""
  pub fn ignored(number: u32) u32 {
      return number + 1;
  }

  pub fn available(number: u32) u32 {
      return ignored(number);
  }
  """

  test "available function works" do
    assert 48 = available(47)
  end

  test "ignored function is not available" do
    refute function_exported?(__MODULE__, :ignored, 0)
  end
end
#module
```