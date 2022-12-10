defmodule ZiglerTest.Types.ArrayTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      :array_float_test,
      {:array_float_binary_test, return: :binary},
      {:array_u8_test, return: :charlists},
      :array_string_test,
      :mut_array_float_test,
      {:mut_array_u8_test, return: :charlists},
      :mut_array_string_test,
      :sentinel_terminated_test,
      :fastlane_beam_term_test,
      :fastlane_erl_nif_term_test,
      {:fastlane_beam_term_ptr_test, return: :noclean},
      {:fastlane_erl_nif_term_ptr_test, return: :noclean}
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

  pub fn array_float_binary_test(passed: [3]f64) [3]f64 {
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

    test "you can pass a binary" do
      assert [2.0, 3.0, 4.0] ==
               array_float_test(<<1.0::float-native, 2.0::float-native, 3.0::float-native>>)
    end

    test "you can get back a binary" do
      assert <<2.0::float-native, 3.0::float-native, 4.0::float-native>> ==
               array_float_binary_test([1.0, 2.0, 3.0])
    end

    test "you can work with u8s" do
      assert [2, 3, 4] == array_u8_test([1, 2, 3])
    end

    test "completely wrong type is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::binary-size(24)>> | list(float | :infinity | :neg_infinity | :NaN) (for `[3]f64`)\n     got: `:bar`\n",
                   fn -> array_float_test(:bar) end
    end

    test "too few elements is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::binary-size(24)>> | list(float | :infinity | :neg_infinity | :NaN) (for `[3]f64`)\n     got: `[1.0, 2.0]`\n     note: length 3 expected but got length 2\n",
                   fn -> array_float_test([1.0, 2.0]) end
    end

    test "too many elements is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::binary-size(24)>> | list(float | :infinity | :neg_infinity | :NaN) (for `[3]f64`)\n     got: `[1.0, 2.0, 3.0, 4.0]`\n     note: length 3 expected but got length 4\n",
                   fn -> array_float_test([1.0, 2.0, 3.0, 4.0]) end
    end

    test "incorrect value types is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::binary-size(24)>> | list(float | :infinity | :neg_infinity | :NaN) (for `[3]f64`)\n     got: `[\"foo\", :bar, :baz]`\n     at index 0:\n     | expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     | got: `\"foo\"`\n",
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
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: binary | list(integer) (for `[3]u8`)\n     got: `\"fo\"`\n     note: binary size 3 expected but got size 2\n",
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

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn fastlane_beam_term_test(env: beam.env, passed: [3]beam.term) [3]beam.term {
    var result: [3]beam.term = undefined;
    for (result) |*item, index| {
      var value: f64 = beam.get(f64, env, passed[index], .{}) catch unreachable;
      item.* = beam.make(env, value + 1.0, .{});
    }
    return result;
  }

  pub fn fastlane_erl_nif_term_test(env: beam.env, passed: [3]e.ErlNifTerm) [3]e.ErlNifTerm {
    var result: [3]e.ErlNifTerm = undefined;
    for (result) |*item, index| {
      var value: f64 = beam.get(f64, env, .{.v = passed[index]}, .{}) catch unreachable;
      item.* = beam.make(env, value + 1.0, .{}).v;
    }
    return result;
  }

  pub fn fastlane_beam_term_ptr_test(env: beam.env, passed: *[3]beam.term) *[3]beam.term {
    for (passed.*) |*item| {
      var value: f64 = beam.get(f64, env, item.*, .{}) catch unreachable;
      item.* = beam.make(env, value + 1.0, .{});
    }
    return passed;
  }

  pub fn fastlane_erl_nif_term_ptr_test(env: beam.env, passed: *[3]e.ErlNifTerm) *[3]e.ErlNifTerm {
    for (passed.*) |*item| {
      var value: f64 = beam.get(f64, env, .{.v = item.*}, .{}) catch unreachable;
      item.* = beam.make(env, value + 1.0, .{}).v;
    }
    return passed;
  }
  """

  describe "fastlanes for" do
    test "beam.term works" do
      assert [2.0, 3.0, 4.0] = fastlane_beam_term_test([1.0, 2.0, 3.0])
    end

    test "e.ErlNifTerm works" do
      assert [2.0, 3.0, 4.0] = fastlane_erl_nif_term_test([1.0, 2.0, 3.0])
    end

    test "beam.term pointer works" do
      assert [2.0, 3.0, 4.0] = fastlane_beam_term_ptr_test([1.0, 2.0, 3.0])
    end

    test "e.ErlNifTerm pointer works" do
      assert [2.0, 3.0, 4.0] = fastlane_erl_nif_term_ptr_test([1.0, 2.0, 3.0])
    end
  end
end
