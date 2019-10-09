defmodule ZiglerTest.GuardsTest do
  use ExUnit.Case

  defmodule GuardedFn do
    # tests more than one zig in the same sigil Z
    use Zigler, app: :zigler

    ~Z"""
    @nif("many_types")
    fn many_types(integer: i64, binary: []u8) i64 {
      return @intCast(i64, binary[@intCast(usize, integer)]);
    }
    """

  end

  test "in its basic mode it works" do
    assert 108 == GuardedFn.many_types(2, "hello")
  end

  test "the first term is guarded" do
    assert_raise FunctionClauseError, fn -> GuardedFn.many_types("bye", "hello") end
  end

  test  "the second term is guarded" do
    assert_raise FunctionClauseError, fn -> GuardedFn.many_types(2, 1) end
  end
end
