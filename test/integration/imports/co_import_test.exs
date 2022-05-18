defmodule ZiglerTest.Imports.CoImportTest do
  use ExUnit.Case, async: true
  use Zig

  ~Z"""
  const co_import_a = @import("co_import_a.zig");
  const co_import_b = @import("co_import_b.zig");

  /// nif: test_47/0
  fn test_47() i64 {
    return co_import_a.co_import_a + co_import_b.co_import_b;
  }
  """

  test "co_imports" do
    assert 47 == test_47()
  end
end
