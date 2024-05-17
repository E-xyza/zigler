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

  pub fn raw_beam_term(env: beam.env, count: c_int, list: [*]const beam.term) beam.term {
    return beam.make(.{.count = count, .item = list[0]}, .{.env = env});
  }

  pub fn raw_erl_nif_term(env: beam.env, count: c_int, list: [*]const e.ErlNifTerm) e.ErlNifTerm {
    return beam.make(.{.count = count, .item = beam.term{.v = list[0]}}, .{.env = env}).v;
  }
  """

  test "raw call with beam.term" do
    assert %{count: 1, item: 1.0} = raw_beam_term(1.0)
  end

  test "raw call with erl_nif_term" do
    assert %{count: 1, item: 1.0} = raw_erl_nif_term(1.0)
  end
end
