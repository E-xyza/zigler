defmodule ZiglerTest.Types.ArrayTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      :array_float_test,
      {:array_u8_test, return: :list},
      :array_string_test,
      :mut_array_float_test,
      {:mut_array_u8_test, return: :list},
      :mut_array_string_test,
      :sentinel_terminated_test
    ]

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

    test "completely wrong type is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list ([3]f64)\n     got: :bar\n",
                   fn -> array_float_test(:bar) end
    end

    test "incorrect number of elements is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list of length 3\n     got length: 2\n",
                   fn -> array_float_test([1.0, 2.0]) end
    end

    test "incorrect value types is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list with elements of type f64 but one of the list items has the wrong type\n",
                   fn -> array_float_test(["foo", :bar, :baz]) end
    end
  end

  describe "for u8s strings are" do
    test "available for used as input" do
      assert ~C"bcd" == array_u8_test("abc")
    end

    test "the default output" do
      assert "bcd" == array_string_test("abc")
    end

    test "not tolerated with wrong length" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: binary of size 3\n     got size: 2\n",
                   fn -> array_string_test("fo") end
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

  pub fn mut_array_u8_test(passed: *[3]u8) *[3]u8 {
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

    test "you can pass a string for a u8 array" do
      assert ~C"bcd" == mut_array_u8_test("abc")
    end

    test "returning a string is default" do
      assert "bcd" == mut_array_string_test("abc")
    end
  end

  ~Z"""
  pub fn sentinel_terminated_test(passed: [3:0]u8) u8 {
    return passed[3];
  }
  """

  describe "sentinel terminated arrays" do
    test "are supported" do
      assert 0 == sentinel_terminated_test("foo")
    end
  end
end
