defmodule ZiglerTest.DryRun.ResourceNameMismatch do
  use Zig, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn foo() i32 { return 47; }

  /// resource: bar definition
  const baz = i64;
  """
end
