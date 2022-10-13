defmodule ZiglerTest.Parser.FileTest do
  use ExUnit.Case, async: true

  alias Zig.Parser

  describe "when given a file with a top-level doc comment" do
    test "a one-liner works" do
      assert %{doc_comment: " hi this is a doc comment\n"} = Parser.parse("""
      //! hi this is a doc comment
      """)
    end

    test "a multiliner works" do
      assert %{doc_comment: " this is line 1\n this is line 2\n"} = Parser.parse("""
      //! this is line 1
      //! this is line 2
      """)
    end
  end

  describe "when given a file with a test" do
    test "one test can be found" do
      assert %{tests: [%{name: "foo"}]} = Parser.parse(~s(test "foo" {}))
    end

    #test "multiple tests can be found" do
    #  assert %{tests: [%{name: "foo"}, %{name: "bar"}]} = Parser.parse(~s(test "foo" {}\ntest "bar" {}))
    #end
  end
end
