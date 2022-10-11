defmodule ZiglerTest.Parser.FileTest do
  use ExUnit.Case, async: true

  alias Zig.Parser

  describe "when given a file" do
    test "it can find a top-level doc comment" do
      assert %{doc_comment: " hi this is a doc comment\n"} = Parser.parse("""
      //! hi this is a doc comment
      """)
    end

    test "it can find a multiline top-level doc comment" do
      assert %{doc_comment: " this is line 1\n this is line 2\n"} = Parser.parse("""
      //! this is line 1
      //! this is line 2
      """)
    end
  end
end
