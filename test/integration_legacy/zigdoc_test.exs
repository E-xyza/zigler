defmodule ZiglerTest.ZigdocTest do

  use ExUnit.Case, async: true

  test "gets the docs" do
    [{AllTheDocs, beam}] = Code.compile_file("test/integration/assets/all_the_docs.exs")
    File.write!("/tmp/.elixir-nifs/allthedocs.beam", beam)
    {:docs_v1, 2, :elixir, "text/markdown", :none, %{}, lst}
      = Code.fetch_docs("/tmp/.elixir-nifs/allthedocs.beam")

    positive_control = lst
    |> Enum.find(&doc_for(&1, :positive_control))
    |> elem(3)

    assert %{"en" => """
    positive_control
    """} = positive_control

    zeroarity_doc = lst
    |> Enum.find(&doc_for(&1, :zeroarity))
    |> elem(3)

    assert %{"en" => "a zero-arity function which returns 47.\n"} = zeroarity_doc

    twoliner = lst
    |> Enum.find(&doc_for(&1, :twoliner))
    |> elem(3)

    assert %{"en" => "this function\nhas two lines of document.\n"} = twoliner
  end

  defp doc_for({{:function, name1, _}, _, _, _, _}, name2), do: name1 == name2
end
