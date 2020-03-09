defmodule ZiglerTest.DryRun.NifArityMismatch do
  use Zigler, dry_run: true

  ~Z"""
  /// nif: foo/1
  fn foo() i32 {
    return 47;
  }
  """

end
