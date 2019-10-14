defmodule ZiglerTest.Parser.ImportTest do
  use ExUnit.Case

  alias Zigler.Parser

  test "we can correctly detect import statements" do
    assert Parser.imports(["""
    const foo = @import("foo.zig")
    """]) == ["foo.zig"]
  end

  test "we can correctly detect multiple import statements" do
    assert Parser.imports(["""
    const foo = @import("foo.zig")
    const bar = @import("bar/baz.zig")
    """]) == ["foo.zig", "bar/baz.zig"]
  end
end
