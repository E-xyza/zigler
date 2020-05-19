defmodule ZiglerTest.DryRun.NifInvalidReturn do
  use Zigler, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn foo() bar {
    return 47;
  }
  """

end
