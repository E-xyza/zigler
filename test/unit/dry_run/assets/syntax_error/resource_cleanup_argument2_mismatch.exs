defmodule ZiglerTest.DryRun.ResourceArgument2Mismatch do
  use Zigler, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn foo() i32 { return 47; }

  /// resource: bar definition
  const bar = i64;

  /// resource: bar cleanup
  fn bar_cleanup(env: beam.env, res: bar) void {
  }
  """

end
