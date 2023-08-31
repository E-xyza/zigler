defmodule ZiglerTest.Types.OptionalTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    leak_check: true,
    otp_app: :zigler

  ~Z"""
  pub fn nullable_integer_test(value: ?u64) ?u64 {
    return if (value) |v| v + 1 else null;
  }

  pub fn nullable_array_test(value: ?[3]u64) ?[3]u64 {
    if (value) |v| {
      var result: [3]u64 = undefined;
      for (v, 0..) |item, index| {
        result[index] = item + 1;
      }
      return result;
    } else return null;
  }

  pub fn nullable_mutable_array_test(value: ?*[3]u64) ?*[3]u64 {
    if (value) |v| {
      for (v.*) | *item | {
        item.* += 1;
      }
      return v;
    } else return null;
  }
  """

  describe "nullable values" do
    test "are passable wrapping integer" do
      assert nil == nullable_integer_test(nil)
      assert 48 == nullable_integer_test(47)
    end

    test "are passable wrapping array" do
      assert nil == nullable_array_test(nil)
      assert [2, 3, 4] == nullable_array_test([1, 2, 3])
    end

    test "are passable wrapping mutable array" do
      assert nil == nullable_mutable_array_test(nil)
      assert [2, 3, 4] == nullable_mutable_array_test([1, 2, 3])
    end
  end
end
