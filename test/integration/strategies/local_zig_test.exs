# tests to make sure we can compile things with a local version of zig.

if System.find_executable("zig") do
  defmodule ZiglerTest.Integration.Strategies.LocalZigTest do
    use ExUnit.Case, async: true

    use Zig, local_zig: true

    ~Z"""
    /// nif: hello/0
    fn hello() void {}
    """

    test "zig can compile with local option" do
      assert nil == hello()
    end
  end
end
