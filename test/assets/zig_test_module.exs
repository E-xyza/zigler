defmodule ZiglerTest.ZigTestModule do
  use Zigler, app: :zigler

  ~Z"""
  const assert = beam.assert;

  /// nif: one/0
  fn one() i64 {
    return 1;
  }

  test "the truth" {
    assert(one() + 1 == 2);
  }
  """
end
