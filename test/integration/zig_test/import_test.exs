defmodule ZiglerTest.Integration.ZigTest.TransitiveTest do
  use ExUnit.Case, async: true
  use Zig
  import Zig.Unit

  alias ZiglerTest.ZigTest.Transitive

  @moduletag :integration
  @moduletag :zigtest

  zigtest(Transitive)

  test "transitive zigtest inclusion via import" do
    assert function_exported?(__MODULE__, :"imported test", 0)
  end

  # removed the test that validated usingnamespace because usingnamespace
  # has changed in 9.0.  usingnamespace does not import indentifiers into
  # the current namespace, but a series of imports into a single namespace.

  test "no inclusion for non-pub imports" do
    refute function_exported?(__MODULE__, :"non-pub test", 0)
    assert Transitive.foo() == 47
  end
end
