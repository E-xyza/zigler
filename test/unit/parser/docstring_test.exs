defmodule ZiglerTest.Parser.DocstringTest do
  # these tests make sure that the parser can correctly identify docstrings.
  use ExUnit.Case, async: true

  alias Zigler.Parser
  alias Zigler.Parser.Function

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

  describe "the nif parser" do
    test "correctly assigns a nif declaration" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0
      """)
      assert %Function{name: :foo, arity: 0} = local
    end

    test "adds in contextual documentation" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0
      """, context: %{local: {:doc, "doc"}})
      assert %Function{doc: "doc"} = local
    end

    test "adds in a long option" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0 long
      """)
      assert %Function{opts: opts} = local
      assert {:long, true} in opts
    end

    test "adds in a dirty_io option" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0 dirty_io
      """)
      assert %Function{opts: opts} = local
      assert {:dirty, :io} in opts
    end

    test "adds in a dirty_cpu option" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0 dirty_cpu
      """)
      assert %Function{opts: opts} = local
      assert {:dirty, :cpu} in opts
    end
  end

  describe "the docstring parser" do
    test "registers a single docstring line" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      """)
      assert {:doc, "foo"} == local
    end

    test "registers multiple docstring lines" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      /// bar
      """)
      assert {:doc, iodata} = local
      assert "foo\nbar" == IO.iodata_to_binary(iodata)
    end

    test "registers a docstring with an extra empty line" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      ///
      """)
      assert {:doc, iodata} = local
      assert "foo\n" == IO.iodata_to_binary(iodata)
    end

    test "registers a multiline docstring" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      ///
      /// bar
      """)
      assert {:doc, iodata} = local
      assert "foo\n\nbar" == IO.iodata_to_binary(iodata)
    end

    test "should error if in the local context of a function" do
      assert_raise CompileError, fn ->
        Parser.parse_docstring("""
        /// foo
        """, context: %{local: %Function{name: :foo, arity: 0}})
      end
    end

    test "registers a function with a docstring" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      /// nif: foo/0
      """)
      assert %Function{name: :foo, arity: 0, doc: "foo"} = local
    end
  end
end
