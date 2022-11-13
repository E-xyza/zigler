defmodule ZiglerTest.Types.ArrayTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [:array_float_test, {:array_u8_test, return: :list}, :array_string_test,
    :mut_array_float_test, {:mut_array_u8_test, return: :list}, :mut_array_string_test]

  ## BASIC ARRAYS

  ~Z"""
  fn common_array_fun(passed: anytype) @TypeOf(passed) {
    var returned : @TypeOf(passed) = undefined;

    for (passed) |value, index| {
      returned[index] = value + 1;
    }

    return returned;
  }

  pub fn array_float_test(passed: [3]f64) [3]f64 {
    return common_array_fun(passed);
  }

  pub fn array_u8_test(passed: [3]u8) [3]u8 {
    return common_array_fun(passed);
  }

  pub fn array_string_test(passed: [3]u8) [3]u8 {
    return common_array_fun(passed);
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

  ## "PSEUDO-MUTABLE" ARRAYS.  Note that "responsibility" for the
  # array data is with the calling function.

  ~Z"""
  fn common_array_mut(passed: anytype) void {
    for (passed.*) |*value| { value.* += 1; }
  }

  pub fn mut_array_float_test(passed: *[3]f64) *[3]f64 {
    common_array_mut(passed);
    return passed;
  }

  pub fn mut_array_u8_test(passed: *[3]f64) *[3]f64 {
    common_array_mut(passed);
    return passed;
  }

  pub fn mut_array_string_test(passed: *[3]u8) *[3]u8 {
    common_array_mut(passed);
    return passed;
  }
  """

  describe "for a generic mutable array" do
    test "you can do a pseudo-mutable pass" do
      assert [2.0, 3.0, 4.0] == mut_array_float_test([1.0, 2.0, 3.0])
      assert [2, 3, 4] == mut_array_u8_test([1, 2, 3])
    end

    test "you can pass a string" do
      assert ~C"bcd" == mut_array_u8_test("abc")
    end

    test "returning a string is default" do
      assert "bcd" == mut_array_string_test("abc")
    end
  end
end
