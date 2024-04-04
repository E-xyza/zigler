defmodule ZiglerTest.Types.RawTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn raw_erl_nif_term(term: e.ErlNifTerm) e.ErlNifTerm {
    var number: f64 = beam.get(f64, .{.v = term}, .{}) catch unreachable;
    return beam.make(number + 1.0, .{}).v;
  }

  pub fn raw_beam_term(term: beam.term) beam.term {
    var number: f64 = beam.get(f64, term, .{}) catch unreachable;
    return beam.make(number + 1.0, .{});
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
