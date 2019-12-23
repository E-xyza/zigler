defmodule ZiglerTest.ImportStdTest do
  #
  # test to see if we can have an import std value
  #

  use Zigler, app: :zigler

  ~Z"""
  const std = @import("std.zig");

  /// nif: trivial/0
  fn trivial() i64 {
    return 0;
  }
  """
end
