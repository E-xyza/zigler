defmodule ZiglerTest.CompileError.InvalidIdentifier do
  use Zig

  ~Z"""
  /// nif: foo/1
  fn foo(_: i64) i64 {
    return not_v;
  }
  """
end
