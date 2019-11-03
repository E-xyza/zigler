defmodule ZiglerTest.Parser.ImportTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser
  alias Zigler.Code

  test "we can correctly detect import statements" do
    assert Parser.tokenize(1, """
    const foo = @import("foo.zig")
    """)
    |> Code.imports == ["foo.zig"]
  end

  test "we can correctly detect multiple import statements" do
    assert Parser.tokenize(1, """
    const foo = @import("foo.zig")
    const bar = @import("bar/baz.zig")
    """)
    |> Code.imports == ["foo.zig", "bar/baz.zig"]
  end
end
