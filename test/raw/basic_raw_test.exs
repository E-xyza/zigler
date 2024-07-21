defmodule ZiglerTest.Raw.BasicRawTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :raw

  use Zig,
    otp_app: :zigler,
    nifs: [
      raw_beam_term: [arity: 1],
      raw_erl_nif_term: [arity: 1]
    ]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn raw_beam_term(env: beam.env, arity: c_int, params: [*]const beam.term) beam.term {
      return beam.make(.{ .arity = arity, .item = params[0] }, .{ .env = env });
  }

  pub fn raw_erl_nif_term(env: beam.env, arity: c_int, params: [*]const e.ErlNifTerm) e.ErlNifTerm {
      return beam.make(.{ .arity = arity, .item = beam.term{ .v = params[0] } }, .{ .env = env }).v;
  }
  """

  test "raw call with beam.term" do
    assert %{arity: 1, item: 1.0} = raw_beam_term(1.0)
  end

  test "raw call with erl_nif_term" do
    assert %{arity: 1, item: 1.0} = raw_erl_nif_term(1.0)
  end
end
