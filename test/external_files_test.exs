defmodule ZiglerTest.ExternalFilesTest do
  use ExUnit.Case

  defmodule RelativeCall do

    # tests that external files relative to current file are okay
    # this will be placed in the build directory in the expected place.

    use Zigler, app: :zigler

    ~Z"""
    const add = @import("support/add.zig");

    @nif("add_one")
    fn add_one(integer: i64) i64 {
      return add.one(integer);
    }
    """
  end

  test "a relative external file can be called" do
    assert 3 == RelativeCall.add_one(2)
  end

  defmodule RecursiveCall do

    # tests that an external file relative to the current file can have
    # a recursive @import statement and still wind up in the correct place.

    use Zigler, app: :zigler

    ~Z"""
    const rec = @import("support/recursive.zig");

    @nif("add_one")
    fn add_one(integer: i64) i64 {
      return rec.add.one(integer);
    }
    """
  end

  test "a recursive external file can be called" do
    assert 3 == RecursiveCall.add_one(2)
  end

  # although elixir will support this, this feature itself appears to be
  # currently disallowed by zig, so we can't safely test it.

  #defmodule RelativeCall do
#
  #  # tests that an external file relative to the current file can have
  #  # a recursive @import statement and still wind up in the correct place.
#
  #  use Zigler, app: :zigler
#
  #  ~Z"""
  #  const rel = @import("support/relative.zig");
#
  #  @nif("add_one")
  #  fn add_one(integer: i64) i64 {
  #    return rel.add.one(integer);
  #  }
  #  """
  #end
#
  #test "a relative external file can be called" do
  #  assert 3 == RelativeCall.add_one(2)
  #end
end
