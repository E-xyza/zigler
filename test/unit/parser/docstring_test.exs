defmodule ZiglerTest.Parser.DocstringTest do
  # these tests make sure that the parser can correctly identify docstrings.
  use ExUnit.Case, async: true

  alias Zigler.Parser

  @moduletag :parser
  @moduletag :docstring

  describe "the docstring_line parser" do
    test "turns a single doc line into a document string" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      """)
      assert {:doc, "foo"} == local
    end

    test "turns a single doc line with spaces into a document string" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo\s\s
      """)
      assert {:doc, "foo"} == local
    end

    test "ignores empty initial document string" do
      assert {:ok, _, _, %Parser{local: nil}, _, _} = Parser.parse_docstring("""
      ///
      """)
    end

    test "ignores empty initial document string with spaces" do
      assert {:ok, _, _, %Parser{local: nil}, _, _} = Parser.parse_docstring("""
      ///\s\s
      """)
    end

    test "correctly postpends two lines together" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// bar
      """, context: %{local: {:doc, "foo"}})
      assert {:doc, iodata} = local
      assert "foo\nbar" == IO.iodata_to_binary(iodata)
    end

    test "correctly postpends an empty line" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      ///
      """, context: %{local: {:doc, "foo"}})
      assert {:doc, iodata} = local
      assert "foo\n" == IO.iodata_to_binary(iodata)
    end
  end

  describe "the docstring parser" do
    test "correctly stores a single docstring line" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      """)
      assert {:doc, "foo"} == local
    end
    test "correctly stores multiple docstring lines" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      /// bar
      """)
      assert {:doc, iodata} = local
      assert "foo\nbar" == IO.iodata_to_binary(iodata)
    end
    test "correctly stores a docstring with an extra empty line" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      ///
      """)
      assert {:doc, iodata} = local
      assert "foo\n" == IO.iodata_to_binary(iodata)
    end
    test "correctly stores a multiline docstring" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      ///
      /// bar
      """)
      assert {:doc, iodata} = local
      assert "foo\n\nbar" == IO.iodata_to_binary(iodata)
    end
  end
end
