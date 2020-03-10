defmodule ZiglerTest.Imports.ImportTest do
  use ExUnit.Case, async: true
  use Zigler

  ~Z"""
  const test_import = @import("test_import.zig");

  /// nif: test_47/0
  fn test_47() i64 {
    return test_import.test_47();
  }
  """

  test "importing inside the zig file is allowed" do
    assert 47 == test_47()
  end

end
