defmodule ZiglerTest.Integration.ZigTest.TransitiveTest do
  use ExUnit.Case, async: true
  use Zig
  import Zig.Unit

  alias ZiglerTest.ZigTest.Transitive

  @moduletag :zigtest

  zigtest(Transitive)

  test "transitive zigtest inclusion via import" do
    assert function_exported?(__MODULE__, :"imported test", 0)
  end

  test "transitive zigtest inclusion via usingnamespace" do
    assert function_exported?(__MODULE__, :"namespaced test", 0)
  end

  test "no inclusion for non-pub imports" do
    refute function_exported?(__MODULE__, :"non-pub test", 0)
    assert Transitive.foo() == 47
  end
end
