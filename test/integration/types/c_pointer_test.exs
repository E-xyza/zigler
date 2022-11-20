defmodule ZiglerTest.Types.CPointerTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ## C pointers as single pointers
  ## C pointers can be a single pointer to a struct, which makes it a "mutable" struct.
  ~Z"""
  pub const TestStruct = extern struct { value: i32 };

  pub fn cpointer_test(passed: [*c]TestStruct) ?i32 {
    if (passed) |unwrapped| {
      return unwrapped.*.value;
    } else {
      return null;
    }
  }
  """

  describe "for a struct cpointer" do
    test "you can pass a map" do
      assert 47 = cpointer_test(%{value: 47})
    end

    test "you can pass null" do
      assert is_nil(cpointer_test(nil))
    end

    test "you can't pass a keyword list" do
      assert_raise ArgumentError, "", fn ->
        cpointer_test(value: 47)
      end
    end
  end

  ~Z"""
  pub fn cpointer_list_test(list: [*c]u8) ?u32 {
    var sum: u32 = 0;
    if (list) |_| {
      for (list[0..3]) |item| {sum += item;}
      return sum;
    } else return null;
  }
  """

  describe "for a list cpointer" do
    test "you can pass a list" do
      assert 6 == cpointer_list_test([1, 2, 3])
    end

    test "you can pass null" do
      assert is_nil(cpointer_list_test(nil))
    end

    test "you can pass a string for u8" do
      assert Enum.sum(~C"abc") == cpointer_list_test("abc")
    end
  end
end
