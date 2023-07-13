defmodule ZiglerTest.Types.SliceNoLeakCheckTest do
  use ZiglerTest.IntegrationCase, async: true

  # this test engages the normal beam allocator.  This is because in MacOS,
  # the dynamic library fails to initialize beam.allocator threadlocal, and
  # behavior is different from linux.

  use Zig,
    otp_app: :zigler,
    nifs: [
      {:slice_u8_test, return: :list},
      ...
    ]

  ## BASIC SLICES
  # note that slices don't implicitly free, if you want to free,
  # you must specify that it frees.

  ~Z"""
  const beam = @import("beam");

  fn common_slice_fun(passed: anytype) @TypeOf(passed) {
    for (passed) |*value| {value.* += 1;}
    return passed;
  }

  pub fn slice_float_test(passed: []f64) []f64 {
    return common_slice_fun(passed);
  }

  pub fn slice_u8_test(passed: []u8) []u8 {
    return common_slice_fun(passed);
  }

  pub fn slice_string_test(passed: []u8) []u8 {
    return common_slice_fun(passed);
  }

  fn common_slice_copy_fun(passed: anytype) @TypeOf(passed) {
    const Child = @typeInfo(@TypeOf(passed)).Pointer.child;

    // catch unreachable because this is just a basic test
    var returned = beam.allocator.alloc(Child, passed.len) catch unreachable;
    for (passed) |value, index| {
      returned[index] = value + 1;
    }
    return returned;
  }

  pub fn freed_slice_float_test(passed: []f64) []f64 {
    return common_slice_copy_fun(passed);
  }

  pub fn freed_slice_u8_test(passed: []u8) []u8 {
    return common_slice_copy_fun(passed);
  }

  pub fn freed_slice_string_test(passed: []u8) []u8 {
    return common_slice_copy_fun(passed);
  }
  """

  describe "for a generic slice" do
    test "you can pass a list" do
      assert [2.0, 3.0, 4.0] == slice_float_test([1.0, 2.0, 3.0])
    end

    test "you can work with u8s" do
      assert [2, 3, 4] == slice_u8_test([1, 2, 3])
    end

    test "completely wrong type is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::_ * 64>> | list(float | :infinity | :neg_infinity | :NaN) (for `[]f64`)\n     got: `:bar`\n",
                   fn -> slice_float_test(:bar) end
    end

    test "incorrect value types is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::_ * 64>> | list(float | :infinity | :neg_infinity | :NaN) (for `[]f64`)\n     got: `[\"foo\", :bar, :baz]`\n     at index 0:\n     | expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     | got: `\"foo\"`\n",
                   fn -> slice_float_test(["foo", :bar, :baz]) end
    end
  end

  describe "for u8s strings are" do
    test "available for used as input" do
      assert ~C"bcd" == slice_u8_test("abc")
    end

    test "the default output" do
      assert "bcd" == slice_string_test("abc")
    end
  end

  ~Z"""
  const e = @import("erl_nif");

  pub fn fastlane_beam_term_test(env: beam.env, passed: []beam.term) []beam.term {
    for (passed) |*item| {
      var value: f64 = beam.get(f64, env, item.*, .{}) catch unreachable;
      item.* = beam.make(env, value + 1.0, .{});
    }
    return passed;
  }

  pub fn fastlane_erl_nif_term_test(env: beam.env, passed: []e.ErlNifTerm) []e.ErlNifTerm {
    for (passed) |*item| {
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
  end

  ~Z"""
  pub fn sentinel_terminated_test(passed: [:0]u8) u8 {
    return passed[3];
  }
  """

  describe "sentinel terminated slices" do
    test "are supported" do
      assert 0 == sentinel_terminated_test("foo")
    end
  end
end
