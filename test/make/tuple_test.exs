defmodule ZiglerTest.Make.TupleTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn make_ok_tuple() beam.term {
      return beam.make(.{ .ok, 47 }, .{});
  }
  """

  test "an :ok tuple can be returned" do
    assert {:ok, 47} = make_ok_tuple()
  end
end
