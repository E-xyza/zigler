defmodule ZiglerTest.Types.NullableTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn nullable_integer_test(value: ?u64) ?u64 {
    return if (value) |v| v + 1 else null;
  }

  pub fn nullable_array_test(value: ?[3]u64) ?[3]u64 {
    if (value) |v| {
      var result: [3]u64 = undefined;
      for (v) |item, index| {
        result[index] = item + 1;
      }
      return result;
    } else return null;
  }
  """

  describe "nullable values" do
    test "are passsable as integer" do
      assert nil == nullable_integer_test(nil)
      assert 48 == nullable_integer_test(47)
    end
  end
end
