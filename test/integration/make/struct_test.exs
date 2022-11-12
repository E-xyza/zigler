defmodule ZiglerTest.Make.StructTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn make_map(env: beam.env) beam.term {
    return beam.make(env, .{.foo = 47, .bar = "test"});
  }
  """

  test "an anonymous struct can be returned as a map" do
    assert %{foo: 47, bar: "test"} = make_map()
  end
end
