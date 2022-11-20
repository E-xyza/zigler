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
      assert nil = cpointer_test(nil)
    end

    test "you can't pass a keyword list" do
      assert_raise ArgumentError, "", fn ->
        cpointer_test(value: 47)
      end
    end
  end
end
