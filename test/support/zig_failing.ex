defmodule ZiglerTest.ZigFailingTestModule do
  use Zigler, app: :zigler

  ~Z"""
  const assert = beam.assert;

  /// nif: one/0
  fn one() i64 {
    return 1;
  }

  test "a lie" {
    try assert(one() == 2);
  }
  """
end


defmodule ZiglerTest.ZigFailingTestShim do
  use Zigler.Unit

  zigtest ZiglerTest.ZigFailingTestModule
end
