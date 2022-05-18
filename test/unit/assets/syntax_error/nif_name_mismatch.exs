defmodule ZiglerTest.DryRun.NifNameMismatch do
  use Zig, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn bar() i32 {
    return 47;
  }
  """
end
