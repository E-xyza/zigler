defmodule ZiglerTest.Raw.ZigCallTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      raw_call_erl_nif_term: [raw: 1]
    ]

  @moduletag :skip

  # Note "raw" calls can't be called in managed threaded or yielding mode.
  # TODO: put warnings on this

  ~Z"""
  const beam = @import("beam");

  pub fn raw_call_erl_nif_term(env: beam.env, count: c_int, list: [*]const beam.term) beam.term {
    return beam.make(.{.count = count, .item = list[0]}, .{.env = env});
  }
  """

  test "raw call" do
    assert %{count: 1, item: 1.0} = raw_call_erl_nif_term(1.0)
  end
end
