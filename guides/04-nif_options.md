# Nif options

Zigler gives you several ways to configure compilation options for nifs. These options are part of
the `use Zig` directive, in the keyword options under the key `:nifs`. This key itself points to a
keyword list where the keys are the names of the nif functions and the values are a keyword list of
options that apply to that function.

This guide shows you all of the nif options except for options related to
[Resources](5-resources.html), [C integration](6-c_integration.html), or
[Concurrency](7-concurrency.html).

## Automatic options (elixir)

To declare that functions should have their options automatically determined, use `...` in the nifs
parameter list. Nifs which sholud have manually decided options should come after the `...` as a
keyword list. If all options are automatically determined, then omitting the `:nif` keyword
completely is valid.

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

Erlang cannot interpret the `...` AST in elixir, so you must use `[auto]` atom in the nifs options
instead.

```erlang
-zig_opts([{nifs, [auto]}])
```

## Return type

In [Collections](2-collections.html) we saw how certain collection types could be manually
marshalled into alternative representations using [`beam.make`](beam.html#make). This can be handled
as a nif configuration as follows. The advantage to doing it this way is that the typespec for the
function will correctly reflect the return type.

```elixir
defmodule ReturnTypeTest do
  use ExUnit.Case, async: true

  use Zig, 
    otp_app: :zigler,
    nifs: [
        returns_binary: [return: :binary],
        returns_list: [return: :list]
    ]

  ~Z"""
  pub fn returns_binary() [3]u16 {
      return [3]u16{47, 48, 49};
  }

  pub fn returns_list() []const u8 {
      return "Hello world!";
  }
  """

  test "returns binary" do
    assert <<47, 0, 48, 0, 49, 0>> = returns_binary()
  end

  test "returns list" do
    assert ~C'Hello world!' = returns_list()
  end
end
```

## Alias

It's possible to create a new function which is an alias of another function. This is how it's done:

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

If you want to disable automatic allocator cleanup of datatypes, you can do so either in the
`:return` or `:args` section by including the `:noclean` option. This is most useful if your args or
return data are going to be persisted beyond the lifetime of the nif call, though doing this in many
cases is not recommended.

This flag can be stacked with previous options, for example: `return: [:noclean, :binary]`.

## Leak Check

It's possible to wrap each function call in its own instance of
[`beam.debug_allocator`](beam.html#debug_allocator) bound into the
[`beam.allocator`](beam.html#allocator) threadlocal variable. If you tag your nif as `leak_check`,
it will check that `beam.allocator` has cleared all of its contents at the end of the function call,
and if that hasn't happened, it raises.

Note that this is currently not supported in windows builds.

```elixir
defmodule LeakCheckTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [check_me: [leak_check: true]]

  ~Z"""
  const beam = @import("beam");
  pub fn check_me() !void {
      _ = try beam.context.allocator.create(u8);
  }
  """

  @tag [erroring: true, no_windows: true]
  test "leak check" do
    require Logger
    Logger.warning("====== the following leak message is expected: =========== START")
    Process.sleep(200)
    assert_raise RuntimeError, "memory leak detected in function `LeakCheckTest.check_me/0`", fn ->
      check_me()
    end
    Logger.warning("=========================================================== END")
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
      _ = try beam.context.allocator.create(u8);
  }
  """

  @tag [erroring: true, no_windows: true]
  test "leak check" do
    require Logger
    Logger.warning("====== the following leak message is expected: =========== START")
    Process.sleep(200)

    assert_raise RuntimeError, "memory leak detected in function `LeakCheckAllTest.check_me/0`", fn ->
      check_me()
    end
    Logger.warning("=========================================================== END")
  end
end
```

## Typespec override

Typespecs generation can be suppressed by setting `spec: false`. If you want typespecs for such
functions, specify using `@spec` elsewhere in your module.

For example:

```elixir
defmodule Override do
  use Zig, 
    otp_app: :zigler,
    nifs: [typespec_override: [spec: false]]

  @spec typespec_override(integer) :: integer
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
#module
```
