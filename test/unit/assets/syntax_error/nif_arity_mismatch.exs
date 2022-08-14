defmodule ZiglerTest.DryRun.NifArityMismatch do
  use Zig

  ~Z"""
  /// nif: foo/1
  fn foo() i32 {
    return 47;
  }
  """
end
