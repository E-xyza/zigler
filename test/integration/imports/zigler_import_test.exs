defmodule ZiglerTest.Imports.ZiglerImportTest do
  #
  # test to make sure that "import" option syntax is correct.
  #

  use ExUnit.Case, async: true
  use Zigler, imports: [:defaults, test_import: "test_import.zig"]

  ~Z"""
  /// nif: test_47/0
  fn test_47() i64 {
    return test_import.test_47();
  }
  """

  test "imports can occur in the zigler settings" do
    assert 47 == test_47()
  end
end
