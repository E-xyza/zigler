defmodule ZiglerTest.ZigT do
  # a module with a single zig test

  use Zigler, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn foo() i32 { return 47; }

  test "foo returns 47" {
    assert(47 == foo());
  }
  """
end


defmodule ZiglerTest.DryRun.UnitTest do
  # checks which make sure our code looks good.
  use ExUnit.Case, async: true

  defmodule OneTest do
    use Zigler, dry_run: true
    import Zigler.Unit

    zigtest ZiglerTest.ZigT
  end

  test "dry run is successful" do
    assert {:"foo returns 47", 0} in OneTest.__info__(:functions)
  end

end

