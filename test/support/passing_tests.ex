defmodule ZiglerTest.PassingTests do
  use Zigler

  ~Z"""
  const assert = beam.assert;

  /// nif: forty_seven/0
  fn forty_seven() i32 {
    return 47;
  }

  test "forty seven returns forty seven" {
    assert(47 == 47);
  }
  """
end
