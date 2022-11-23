defmodule ZiglerTest.Types.RawTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn raw_erl_nif_term(env: beam.env, term: e.ErlNifTerm) e.ErlNifTerm {
    var number: f64 = beam.get(f64, env, .{.v = term}) catch unreachable;
    return beam.make(env, number + 1.0, .{}).v;
  }

  pub fn raw_beam_term(env: beam.env, term: beam.term) beam.term {
    var number: f64 = beam.get(f64, env, term) catch unreachable;
    return beam.make(env, number + 1.0, .{});
  }
  """

  # TODO: make sure that the "count" thing is the type we expect.

  describe "raw call" do
    test "with e.ErlNifTerm" do
      assert 2.0 = raw_erl_nif_term(1.0)
    end

    test "with beam.term" do
      assert 2.0 = raw_beam_term(1.0)
    end
  end
end
