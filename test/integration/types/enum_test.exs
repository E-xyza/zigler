defmodule ZiglerTest.Types.EnumTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    local_zig: true

  ~Z"""
  pub const EnumType = enum{ foo, bar };

  pub fn untagged_swap(value: EnumType) EnumType {
    return if (value == .foo) .bar else .foo;
  }
  """

  describe "given an enum" do
    test "you can pass in atoms to get the value out" do
      assert :foo = untagged_swap(:bar)
      assert :bar = untagged_swap(:foo)
    end

    test "you can pass in integers and they'll be coerced" do
      assert :foo = untagged_swap(1)
      assert :bar = untagged_swap(0)
    end

    test "if you try to use something that isn't an atom or integer" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: atom or integer (for EnumType)\n     got: \"foo\"\n",
                   fn -> untagged_swap("foo") end
    end

    test "if you try to use an invalid atom" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     :zag is not a valid atom for EnumType (should be one of [:bar, :foo])\n",
                   fn -> untagged_swap(:zag) end
    end

    test "if you try to use an invalid number" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     42 is not a valid atom for EnumType (should be one of [0, 1])\n",
                   fn -> untagged_swap(42) end
    end
  end
end
