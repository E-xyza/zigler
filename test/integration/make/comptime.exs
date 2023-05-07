defmodule ZiglerTest.Make.ComptimeTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn make_comptime_int(env: beam.env) beam.term {
    return beam.make(env, 47, .{});
  }

  pub fn make_comptime_float(env: beam.env) beam.term {
    return beam.make(env, 47.0, .{});
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
