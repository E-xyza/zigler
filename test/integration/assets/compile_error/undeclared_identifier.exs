defmodule ZiglerTest.CompileError.InvalidIdentifier do
  use Zigler

  ~Z"""
  /// nif: foo/1
  fn foo(v: i64) i64 {
    return not_v;
  }
  """
end
