defmodule ZiglerTest.Make.StructTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn make_map() beam.term {
    return beam.make(.{.foo = 47, .bar = "test"}, .{});
  }
  """

  test "an anonymous struct can be returned as a map" do
    assert %{foo: 47, bar: "test"} = make_map()
  end
end
