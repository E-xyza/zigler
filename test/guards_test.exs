defmodule ZiglerTest.GuardsTest do
  use ExUnit.Case

  defmodule GuardedFn do
    # tests more than one zig in the same sigil Z
    use Zigler, app: :zigler

    ~Z"""
    /// nif: many_types/2
    fn many_types(integer: i64, binary: []u8) i64 {
      return @intCast(i64, binary[@intCast(usize, integer)]);
    }

    /// nif: first_int/1
    fn first_int(list: []i64) i64 {
      return list[0];
    }
    """

  end

  test "in its basic mode it works" do
    assert 108 == GuardedFn.many_types(2, "hello")
  end

  test "the first term is guarded" do
    assert_raise FunctionClauseError, fn -> GuardedFn.many_types("bye", "hello") end
  end

  test "the second term is guarded" do
    assert_raise FunctionClauseError, fn -> GuardedFn.many_types(2, 1) end
  end

  test "lists of integers are guarded" do
    assert 1 == GuardedFn.first_int([1, 2])
    assert_raise FunctionClauseError, fn -> GuardedFn.first_int(:hello) end
    assert_raise FunctionClauseError, fn -> GuardedFn.first_int([2, :hello]) end
  end
end
