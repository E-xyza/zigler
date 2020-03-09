defmodule ZiglerTest.DryRun.EmptySigilZ do
  use Zigler, dry_run: true

  ~Z"""
  fn foo() i64 {
    return 47;
  }
  """

end
