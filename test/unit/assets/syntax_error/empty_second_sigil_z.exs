defmodule ZiglerTest.DryRun.EmptySigilZ do
  use Zigler, dry_run: true

  ~Z"""
  /// nif: bar/0
  fn bar() i64 {
    return 47;
  }
  """

  ~Z"""
  fn foo() i64 {
    return 47;
  }
  """

end
