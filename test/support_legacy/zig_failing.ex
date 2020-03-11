defmodule ZiglerTest.ZigFailingTestModule do

  @moduledoc false

  use Zigler, otp_app: :zigler

  ~Z"""
  const assert = beam.assert;

  /// nif: one/0
  fn one() i64 {
    return 1;
  }

  test "a lie" {
    assert(one() == 2);
  }
  """
end

defmodule ZiglerTest.ZigFailingTestShim do

  @moduledoc false

  use Zigler.Unit

  zigtest ZiglerTest.ZigFailingTestModule
end
