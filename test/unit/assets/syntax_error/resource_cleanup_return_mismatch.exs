defmodule ZiglerTest.DryRun.ResourceReturnMismatch do
  use Zig

  ~Z"""
  /// nif: foo/0
  fn foo() i32 { return 47; }

  /// resource: bar definition
  const bar = i64;

  /// resource: bar cleanup
  fn bar_cleanup(env: beam.env, res: *bar) i64 {
    return 47;
  }
  """
end
