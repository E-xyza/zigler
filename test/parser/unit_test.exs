defmodule ZiglerTest.Parser.UnitTest do
  use ExUnit.Case, async: true

  alias Zig.Parser

  describe "the identifier parser" do
    test "catches basic identifiers" do
      assert {:ok, ["foo"], "", _, _, _} = Parser.identifier("foo")
    end

    test "catches capitalized identifiers" do
      assert {:ok, ["FOO"], "", _, _, _} = Parser.identifier("FOO")
    end

    test "catches identifiers with underscores" do
      assert {:ok, ["foo_bar"], "", _, _, _} = Parser.identifier("foo_bar")
    end

    test "catches identifiers with numbers" do
      assert {:ok, ["foo4bar"], "", _, _, _} = Parser.identifier("foo4bar")
    end

    test "does not accept identifiers starting with numbers" do
      assert {:error, _msg, "4foo", _, _, _} = Parser.identifier("4foo")
    end

    test "leaves behind stuff after whitespace" do
      assert {:ok, ["foo"], " bar", _, _, _} = Parser.identifier("foo bar")
      assert {:ok, ["foo"], "\nbar", _, _, _} = Parser.identifier("foo\nbar")
    end
  end
end
