# defmodule ZiglerTest.RawCallTest do
#  use ExUnit.Case, async: true
#
#  use Zig,
#    otp_app: :zigler,
#    import: [
#      raw_call_erl_nif_term: :raw
#    ]
#
#  ~Z"""
#  const beam = @import("beam");
#  const e = @import("erl_nif");
#
#  pub fn raw_call_erl_nif_term(env: beam.env, count: usize, list: [*c]e.ErlNifTerm) e.ErlNifTerm {
#    return beam.make(env, .{.count = count, .item = list[0]}).v;
#  }
#  """
#
#  # TODO: make sure that the "count" thing is the type we expect.
#
#  test "raw call" do
#    assert {1, 1.0} = raw_call_erl_nif_term(1.0)
#  end
# end
