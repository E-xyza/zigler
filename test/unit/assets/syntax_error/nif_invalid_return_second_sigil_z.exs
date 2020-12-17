defmodule ZiglerTest.DryRun.NifInvalidReturnSecondSigilZ do
  use Zig, dry_run: true

  ~Z"""
  /// nif: baz/0
  fn baz() i64 {
    return 47;
  }
  """

  # just put this here to pad more "other lines"
  def quux, do: 47

  ~Z"""
  /// nif: foo/0
  fn foo() bar {
    return 47;
  }
  """

end
