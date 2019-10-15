defmodule ZigTest.ZigTestModule do
  use Zigler, app: :zigler

  ~Z"""
  const assert = @import("std").debug.assert;

  @nif("one")
  fn one() i64 {
    return 1;
  }

  test "the truth" {
    assert(one() + 1 == 2);
  }
  """
end
