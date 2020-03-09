defmodule ZiglerTest.DryRun.NifNameMismatch do
  use Zigler, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn bar() i32 {
    return 47;
  }
  """

end
