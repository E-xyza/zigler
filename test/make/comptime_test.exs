defmodule ZiglerTest.Make.ComptimeTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn make_comptime_int() beam.term {
      return beam.make(47, .{});
  }

  pub fn make_comptime_float() beam.term {
      return beam.make(47.0, .{});
  }
  """

  describe "comptime" do
    test "ints can be returned" do
      assert 47 = make_comptime_int()
    end

    test "floats can be returned" do
      assert 47.0 == make_comptime_float()
    end
  end
end
