defmodule ZiglerTest.Types.OptionalTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler

  ~Z"""
  pub fn nullable_integer_test(value: ?u64) ?u64 {
      return if (value) |v| v + 1 else null;
  }

  pub fn nullable_bool_test(value: ?bool) ?bool {
      return if (value) |v| !v else null;
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

  const E = enum { foo, bar };

  pub fn nullable_enum_test(value: ?E) ?E {
      return value;
  }

  pub fn nullable_mutable_array_test(value: ?*[3]u64) ?*[3]u64 {
      if (value) |v| {
          for (v) |*item| {
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

    test "are passable wrapping bool" do
      assert false == nullable_bool_test(true)
      assert nil == nullable_bool_test(nil)
    end

    test "are passable wrapping array" do
      assert nil == nullable_array_test(nil)
      assert [2, 3, 4] == nullable_array_test([1, 2, 3])
    end

    test "are passable wrapping enum" do
      assert nil == nullable_enum_test(nil)
      assert :foo == nullable_enum_test(:foo)
    end

    test "are passable wrapping mutable array" do
      assert nil == nullable_mutable_array_test(nil)
      assert [2, 3, 4] == nullable_mutable_array_test([1, 2, 3])
    end
  end

  test "argument error if something wrong is sent" do
    # note that integers are wrongly typed.  You must explicitly send an integer or nil.
    assert_raise ArgumentError,
                 "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: nil | integer (for `?u64`)\n     got: `:foo`\n     note: ?u64 can take the atom `nil` but no other atom\n",
                 fn -> nullable_integer_test(:foo) end
  end
end
