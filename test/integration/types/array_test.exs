defmodule ZiglerTest.Types.ArrayTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [:array_float_test, {:array_u8_test, return: :list}, :array_string_test]

  ~Z"""
  fn common_array_fun(comptime T: type, passed: T) T {
    var returned : T = undefined;

    for (passed) |value, index| {
      returned[index] = value + 1;
    }

    return returned;
  }

  pub fn array_float_test(passed: [3]f64) [3]f64 {
    return common_array_fun([3]f64, passed);
  }

  pub fn array_u8_test(passed: [3]u8) [3]u8 {
    return common_array_fun([3]u8, passed);
  }

  pub fn array_string_test(passed: [3]u8) [3]u8 {
    return common_array_fun([3]u8, passed);
  }
  """

  describe "for a generic array" do
    test "you can pass a list" do
      assert [2.0, 3.0, 4.0] == array_float_test([1.0, 2.0, 3.0])
    end

    test "you can work with u8s" do
      assert [2, 3, 4] == array_u8_test([1, 2, 3])
    end
  end

  describe "for u8s strings are" do
    test "available for used as input" do
      assert ~C"bcd" == array_u8_test("abc")
    end

    test "the default output" do
      assert "bcd" == array_string_test("abc")
    end
  end
end
