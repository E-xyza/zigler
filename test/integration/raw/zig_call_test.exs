defmodule ZiglerTest.Raw.ZigCallTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :raw
  test "restore"

#  use Zig,
#    otp_app: :zigler
#
#  # Note "raw" calls can't be called in managed threaded or yielding mode.
#  # TODO: put warnings on this
#
#  ~Z"""
#  const beam = @import("beam");
#  const e = @import("erl_nif");
#
#  pub fn raw_beam_term(env: beam.env, count: c_int, list: [*]const beam.term) beam.term {
#    return beam.make(.{.count = count, .item = list[0]}, .{.env = env});
#  }
#
#  pub fn raw_erl_nif_term(env: beam.env, count: c_int, list: [*]const e.erl_nif_term) beam.term {
#    return beam.make(.{.count = count, .item = list[0]}, .{.env = env});
#  }
#  """
#
#  test "raw call with beam.term" do
#    assert %{count: 1, item: 1.0} = raw_beam_term(1.0)
#  end
#
#  test "raw call with erl_ttif_term" do
#    assert %{count: 1, item: 1.0} = raw_erl_nif_term(1.0)
#  end
end
