defmodule ZiglerTest.DryRun.ResourceNameMismatch do
  use Zigler, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn foo() i32 { return 47; }

  /// resource: bar definition
  const baz = i64;
  """

end
