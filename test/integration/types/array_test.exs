defmodule ZiglerTest.Types.ArrayTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler

  ~Z"""
  pub fn array_float_test(passed: [3]f64) [3]f64 {
    var returned : [3]f64 = undefined;

    for (passed) |value, index| {
      returned[index] = value + 1.0;
    }

    return returned;
  }

  pub fn array_string_test(passed: [3]u8) [3]u8 {
    var returned : [3]u8 = undefined;

    for (passed) |value, index| {
      returned[index] = value + 1;
    }

    return returned;
  }
  """

  describe "for a generic array" do
    test "you can pass a list" do
      assert [2.0, 3.0, 4.0] == array_float_test([1.0, 2.0, 3.0])
    end
  end
end
