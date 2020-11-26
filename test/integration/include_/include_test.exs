defmodule ZiglerTest.Include.IncludeTest do
  use ExUnit.Case, async: true
#  use Zigler
#
#  ~Z"""
#  const c = @cImport({
#    @cInclude("fortyseven.h");
#  });
#
#  /// nif: fortyseven/0
#  fn fortyseven() c_int {
#    return c.FORTYSEVEN;
#  }
#  """

  test "single include works" #do
#    assert 47 == fortyseven()
#  end
end
