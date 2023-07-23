# Nif options

Zigler gives you several ways to configure compilation options for nifs.  These
options are part of the `use Zig` directive, in the keyword options under the 
key `:nifs`.  This key itself points to a keyword list where the keys are the
names of the nif functions and the values are a keyword list of options that
apply to that function.

This guide shows you all of the nif options except for options related to 
[C integration](6-c_integration.html).

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

  pub fn raw_call_beam(env: beam.env, count: c_int, list: [*]const beam.term) beam.term {
    return beam.make(env, .{.count = count, .item = list[0]}, .{});
  }

  pub fn raw_call_erl_nif(env: beam.env, count: c_int, list: [*]const e.ErlNifTerm) e.ErlNifTerm {
    return beam.make(env, .{.count = count, .item = beam.term{.v = list[0]}}, .{}).v;
  }
  """

  test "raw call with beam package" do
    assert %{count: 1, item: {:foo, "bar"}} = raw_call_beam({:foo, "bar"})
  end

  test "raw call with erl_nif package" do
    assert %{count: 1, item: {:foo, "bar"}} = raw_call_erl_nif({:foo, "bar"})
  end
end

# modules
```

Note that either the `beam.term` or the `e.ErlNifTerm` can be used.