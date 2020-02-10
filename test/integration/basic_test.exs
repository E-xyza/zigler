defmodule ZiglerTest.Integration.BasicTest do
  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  /// nif: fortyseven/0
  fn fortyseven() i64 {
    return 47;
  }
  """

  test "zero-arity function works" do
    assert 47 == fortyseven()
  end

end
