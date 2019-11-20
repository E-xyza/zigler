defmodule ZigTest.ZigTestModule do
  use Zigler, app: :zigler

  ~Z"""
  const assert = @import("std").debug.assert;

  /// nif: one/0
  fn one() i64 {
    return 1;
  }

  test "the truth" {
    assert(1 == 2);
    // assert(one() + 1 == 2);
  }
  """
end
