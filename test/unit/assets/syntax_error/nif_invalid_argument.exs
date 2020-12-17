defmodule ZiglerTest.DryRun.NifInvalidArgument do
  use Zig, dry_run: true

  ~Z"""
  /// nif: foo/1
  fn foo(bar: baz) i64 {
    return 47;
  }
  """

end
