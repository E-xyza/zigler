defmodule ZiglerTest.Parser.InitializerTest do
  # these tests make sure that the parser can correctly identify docstrings.
  use ExUnit.Case, async: true

  alias Zigler.Parser

  @moduletag :parser

  describe "the parser intializer" do
    test "produces a parser object" do
      {:ok, [], _rest, context, _, _} = Parser.parser_initializer("")
      assert %Parser{} = context
    end

    test "can be preseeded" do
      {:ok, [], _rest, context, _, _} =
        Parser.parser_initializer("", context: %{local: {:doc, "foo"}})
      assert %Parser{local: {:doc, "foo"}} = context
    end
  end

end
