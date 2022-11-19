defmodule ZiglerTest.Types.ManypointerTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      :manypointer_float_test,
      {:manypointer_u8_test, return: :list},
      :manypointer_string_test,
      :sentinel_terminated_test,
      :sentinel_terminated_return_test,
      :sentinel_terminated_binary_return_test,
      {:sentinel_terminated_u8_list_return_test, return: :list},
      #:fastlane_beam_term_test,
      #:fastlane_erl_nif_term_test
    ]

  ## BASIC MULTIPOINTERS
  # note that the length must be communicated some other way.
  # in this case we just assume it's 3
  # also note that manypointers are implicitly mut.

  ~Z"""
  fn childType(comptime T: type) type { return @typeInfo(T).child; }

  fn common_array_sum(comptime T: type, passed: anytype) T {
    var sum: T = 0;
    for (passed[0..3]) |value| {sum += value;}
    return sum;
  }

  pub fn manypointer_float_test(passed: [*]f64) f64 {
    return common_array_sum(f64, passed);
  }

  pub fn manypointer_u8_test(passed: [*]u8) u32 {
    return common_array_sum(u32, passed);
  }

  pub fn manypointer_string_test(passed: [*]u8) u32 {
    return common_array_sum(u32, passed);
  }
  """

  describe "for a generic manypointer" do
    test "you can pass a list" do
      assert 6.0 == manypointer_float_test([1.0, 2.0, 3.0])
    end

    test "you can work with u8s" do
      assert 6 == manypointer_u8_test([1, 2, 3])
    end

    test "completely wrong type is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list ([*]f64)\n     got: :bar\n",
                   fn -> manypointer_float_test(:bar) end
    end

    test "incorrect value types is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list ([*]f64) but one of the list items (in [\"foo\", :bar, :baz]) has the wrong type\n",
                   fn -> manypointer_float_test(["foo", :bar, :baz]) end
    end
  end

  describe "for u8s strings are" do
    test "available for used as input" do
      assert Enum.sum(~C'abc') == manypointer_u8_test("abc")
    end
  end

  describe "for normal manypointers" do
    test "having a return value is prohibited" do
      assert_raise CompileError,  " functions returning [*]u8 are not allowed", fn ->
        Code.compile_file("_manypointer_forbidden_output.exs", __DIR__)
      end
    end

    test "correct line and file"
  end

  ~Z"""
  pub fn sentinel_terminated_test(passed: [*:0]u8) u8 {
    return passed[3];
  }

  var str_result = [_]f64{47.0, 0.0};

  pub fn sentinel_terminated_return_test() [*:0.0]f64 {
    return @ptrCast([*:0.0]f64, &str_result);
  }

  var stbr_result = [_]u8{'b', 'a', 'r', 0};

  pub fn sentinel_terminated_binary_return_test() [*:0]u8 {
    return @ptrCast([*:0]u8, &stbr_result);
  }

  pub fn sentinel_terminated_u8_list_return_test() [*:0]u8 {
    return @ptrCast([*:0]u8, &stbr_result);
  }
  """

  describe "sentinel terminated arrays" do
    test "are supported" do
      assert 0 == sentinel_terminated_test("foo")
    end

    test "can be returned" do
      assert [47.0] == sentinel_terminated_return_test()
    end

    test "can be returned as binary" do
      assert "bar" == sentinel_terminated_binary_return_test()
    end

    test "can be returned as u8 list" do
      assert ~C'bar' == sentinel_terminated_u8_list_return_test()
    end
  end

  #~Z"""
  #const beam = @import("beam");
  #const e = @import("erl_nif");
#
  #pub fn fastlane_beam_term_test(env: beam.env, passed: [3]beam.term) [3]beam.term {
  #  var result: [3]beam.term = undefined;
  #  for (result) |*item, index| {
  #    var value: f64 = beam.get(f64, env, passed[index]) catch unreachable;
  #    item.* = beam.make(env, value + 1.0);
  #  }
  #  return result;
  #}
#
  #pub fn fastlane_erl_nif_term_test(env: beam.env, passed: [3]e.ErlNifTerm) [3]e.ErlNifTerm {
  #  var result: [3]e.ErlNifTerm = undefined;
  #  for (result) |*item, index| {
  #    var value: f64 = beam.get(f64, env, .{.v = passed[index]}) catch unreachable;
  #    item.* = beam.make(env, value + 1.0).v;
  #  }
  #  return result;
  #}
  #"""
#
  #describe "fastlanes for" do
  #  test "beam.term works" do
  #    assert [2.0, 3.0, 4.0] = fastlane_beam_term_test([1.0, 2.0, 3.0])
  #  end
#
  #  test "e.ErlNifTerm works" do
  #    assert [2.0, 3.0, 4.0] = fastlane_erl_nif_term_test([1.0, 2.0, 3.0])
  #  end
#
  #  test "beam.term pointer works" do
  #    assert [2.0, 3.0, 4.0] = fastlane_beam_term_ptr_test([1.0, 2.0, 3.0])
  #  end
#
  #  test "e.ErlNifTerm pointer works" do
  #    assert [2.0, 3.0, 4.0] = fastlane_erl_nif_term_ptr_test([1.0, 2.0, 3.0])
  #  end
  #end
end
