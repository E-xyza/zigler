defmodule ZiglerTest.Integration.DocumentationTest do
  # checks which make sure documentation generation is OK.

  use ExUnit.Case, async: true

  test "documentation works" do
    {:docs_v1, _, _, _, _, _, docs} = Code.fetch_docs(ZiglerTest.Integration.Documentation)

    assert Enum.any?(
             docs,
             &match?(
               {{:function, :foo, 0}, _, _, %{"en" => "documentation for foo"}, _},
               &1
             )
           )
  end
end
