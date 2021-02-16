defmodule ZiglerTest.Imports.ImportTest do
  use ExUnit.Case, async: true
  use Zig

  # basic importing
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

  # importing through subdirectories
  ~Z"""
  const test_subdir_import = @import("subdir/test_import.zig");

  /// nif: test_48/0
  fn test_48() i64 {
    return test_subdir_import.test_48();
  }
  """

  test "importing from subdirectories is allowed" do
    assert 48 == test_48()
  end

  # repeat importation
  ~Z"""
  const test_re_import = @import("subdir/test_import.zig");

  /// nif: test_re_48/0
  fn test_re_48() i64 {
    return test_re_import.test_48();
  }
  """

  test "re importing from subdirectories is allowed" do
    assert 48 == test_re_48()
  end

  # transitive importation
  ~Z"""
  const test_transitive_import = @import("transitive.zig");

  /// nif: transitive_47/0
  fn transitive_47() i64 {
    return test_transitive_import.test_47();
  }
  """

end
