# Raw nifs

All of the nifs shown to this point involve zigler constructing an adapter function with
term marshalling automatically generated.  It is also possible to run a nif without
doing any of those steps.

The normal (C) header for a BEAM nif is as follows:

`static ERL_NIF_TERM hello(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])`

Zigler can interpret certain headers as intended for being called as a raw nif.

## Allowed Signatures

The following two zig function signatures are interpreted as raw nifs:

- `fn (beam.env, c_int, [*]beam.term) beam.term`
- `fn (beam.env, c_int, [*]e.ErlNifTerm) e.ErlNifTerm`

## nif options setup

A raw nif MUST contain the `arity` option.  This can be one of:

- a single integer, representing the desired arity of the nif function
- a single range, representing a range of arities for the nif function
- a list of integers and rannges, representing all the arities of the nif function.

### Example

```elixir
defmodule RawCallTest do
  use ExUnit.Case, async: true

  use Zig, 
    otp_app: :zigler,
    nifs: [
        raw_call_beam: [arity: 1],
        raw_call_erl_nif: [arity: 1],
        raw_call_multi_arity: [arity: [0, 2..3]]
    ]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn raw_call_beam(env: beam.env, count: c_int, list: [*]const beam.term) beam.term {
      return beam.make(.{.count = count, .item = list[0]}, .{.env = env});
  }

  pub fn raw_call_erl_nif(env: beam.env, count: c_int, list: [*]const e.ErlNifTerm) e.ErlNifTerm {
      return beam.make(.{.count = count, .item = beam.term{.v = list[0]}}, .{.env = env}).v;
  }

  pub fn raw_call_multi_arity(env: beam.env, arity: c_int, _: [*]const beam.term) beam.term {
      return beam.make(arity, .{.env = env});
  }
  """

  test "raw call with beam format" do
    assert %{count: 1, item: {:foo, "bar"}} = raw_call_beam({:foo, "bar"})
  end
  
  test "raw call with erl_nif format" do
    assert %{count: 1, item: {:foo, "bar"}} = raw_call_erl_nif({:foo, "bar"})
  end

  test "raw call with multiple arities" do
    assert 0 = raw_call_multi_arity()
    assert 2 = raw_call_multi_arity(:foo, :bar)
    assert 3 = raw_call_multi_arity(:foo, :bar, :baz)

    refute function_exported?(__MODULE__, :raw_call_multi_arity, 1)
  end
end
#module
```

> ### beam.make and beam.get in raw nifs {: .warning}
>
> Note that you MUST supply `.{.env = env}` in the options to beam.make or beam.get calls in raw nifs,
> or functions called by raw nifs. The threadlocal `beam.context` variable which normally stores the
> environment is not set when you make a raw call.
