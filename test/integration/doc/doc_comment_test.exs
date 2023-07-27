defmodule ZiglerTest.Unit.Typespec.DocCommentTest do
  use ExUnit.Case, async: true

  test "it is possible to write a doc comment" do
    assert {:docs_v1, _, _, _, _, _, funcs} = Code.fetch_docs(ZiglerTest.Documentation)

    assert doc =
             Enum.find_value(funcs, fn
               {{:function, :do_something, 1}, _, _, %{"en" => doc}, _} ->
                 doc

               _ ->
                 nil
             end)

    assert IO.iodata_to_binary(doc) =~ "This is a function that does something."
  end
end
