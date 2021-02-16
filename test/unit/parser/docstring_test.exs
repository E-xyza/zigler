defmodule ZiglerTest.Parser.DocstringTest do
  # these tests make sure that the parser can correctly identify docstrings.
  use ExUnit.Case, async: true

  alias Zig.Parser
  alias Zig.Parser.Nif
  alias Zig.Parser.Resource

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
      assert %Nif{name: :foo, arity: 0} = local
    end

    test "adds in contextual documentation" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0
      """, context: %{local: {:doc, "doc"}})
      assert %Nif{doc: "doc"} = local
    end

    test "detects yielding concurrency" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0 yielding
      """)
      assert %Nif{opts: opts} = local
      assert :yielding == opts[:concurrency]
    end

    test "detects threaded concurrency" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0 threaded
      """)
      assert %Nif{opts: opts} = local
      assert :threaded == opts[:concurrency]
    end

    test "detects dirty_io concurrency" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0 dirty_io
      """)
      assert %Nif{opts: opts} = local
      assert :dirty_io == opts[:concurrency]
    end

    test "detects dirty_cpu concurrency" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_nif_declaration("""
      /// nif: foo/0 dirty_cpu
      """)
      assert %Nif{opts: opts} = local
      assert :dirty_cpu ==  opts[:concurrency]
    end
  end

  describe "the resource parser" do
    @describetag :resource
    test "correctly assigns a resource declaration" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_resource_declaration("""
      /// resource: foo definition
      """)
      assert %Resource{name: :foo} = local
    end

    test "adds in contextual documentation" do
      assert {:ok, _, _, %Parser{local: local}, _, _} = Parser.parse_resource_declaration("""
      /// resource: foo definition
      """, context: %{local: {:doc, "doc"}})
      assert %Resource{doc: "doc"} = local
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
      assert_raise SyntaxError, fn ->
        Parser.parse_docstring("""
        /// foo
        """, context: %{local: %Nif{name: :foo, arity: 0}})
      end
    end

    test "registers a function with a docstring" do
      assert {:ok, [], _, %Parser{local: local}, _, _} = Parser.parse_docstring("""
      /// foo
      /// nif: foo/0
      """)
      assert %Nif{name: :foo, arity: 0, doc: "foo"} = local
    end
  end
end
