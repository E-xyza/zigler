defmodule ZiglerTest.Make.StructTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  defstruct [:foo]

  ~Z"""
  const beam = @import("beam");

  pub fn make_map() beam.term {
      return beam.make(.{ .foo = 47, .bar = "test" }, .{});
  }
  """

  test "an anonymous struct can be returned as a map" do
    assert %{foo: 47, bar: "test"} = make_map()
  end

  ~z"""
  pub fn make_struct() beam.term {
      return beam.make(.{.foo =47}, .{.@"struct" = .@"#{__MODULE__}"});
  }
  """

  test "elixir-structs can be installed" do
    assert %__MODULE__{foo: 47} = make_struct()
  end
end
