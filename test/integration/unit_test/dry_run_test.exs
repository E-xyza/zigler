defmodule ZiglerTest.ZigT do
  # a module with a single zig test

  use Zig, dry_run: true

  ~Z"""
  /// nif: foo/0
  fn foo() i32 { return 47; }

  test "foo returns 47" {
    assert(47 == foo());
  }
  """
end

defmodule ZiglerTest.Integration.UnitTest.DryRunTest do
  # checks which make sure our code looks good.
  use ExUnit.Case, async: true

  # we don't suffix this with "Test" so it will be a normal
  # elixir module, and its failure (due to not being loaded) is
  # immaterial.

  defmodule NotExecuted do
    #use ExUnit.Case, async: true
    use Zig, dry_run: true
    import Zig.Unit

    zigtest ZiglerTest.ZigT, dry_run: true
  end

  test "dry run is successful" do
    assert {:"foo returns 47", 0} in NotExecuted.__info__(:functions)
  end

end
