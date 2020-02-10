defmodule ZiglerTest.Parser.FunctionHeaderTest do
  # these tests make sure that the parser can correctly identify docstrings.
  use ExUnit.Case, async: true

  alias Zigler.Parser
  alias Zigler.Parser.Nif

  @moduletag :parser
  @moduletag :function

  describe "the parameter parser" do
    test "correctly parses a basic identifier parameter" do
      assert {:ok, ["i64"], _, _, _, _} = Parser.parse_parameter("foo: i64")
    end

    test "correctly parses a namespaced identifier parameter" do
      assert {:ok, ["beam.env"], _, _, _, _} = Parser.parse_parameter("foo: beam.env")
    end

    test "correctly parses an ErlNifEnv identifier parameter" do
      assert {:ok, ["?*e.ErlNifEnv"], _, _, _, _} = Parser.parse_parameter("foo: ?*e.ErlNifEnv")
    end
  end

  describe "the parameter list parser" do
    test "correctly parses an empty parameter list" do
      assert {:ok, [], _, _, _, _} = Parser.parse_parameter_list("")
    end

    test "correctly parses a parameter list with space" do
      assert {:ok, [], _, _, _, _} = Parser.parse_parameter_list(" ")
    end

    test "correctly parses a parameter list with a single def" do
      assert {:ok, ["i64"], _, _, _, _} = Parser.parse_parameter_list("foo: i64")
    end

    test "correctly parses a parameter list with a multiple def" do
      assert {:ok, ["i64", "f64"], _, _, _, _} = Parser.parse_parameter_list("foo: i64, bar: f64")
    end
  end

  describe "the function header parser" do
    test "correctly obtains the retval" do
      assert {:ok, _, _, context, _, _} = Parser.parse_function_header("""
        fn foo() i64 {
      """, context: %{local: %Nif{name: :foo, arity: 0}})

      assert %Parser{global: [function], local: nil} = context
      assert %Nif{name: :foo, arity: 0, retval: "i64"} = function
    end

    test "correctly obtains zero parameters" do
      assert {:ok, _, _, context, _, _} = Parser.parse_function_header("""
        fn foo() i64 {
      """, context: %{local: %Nif{name: :foo, arity: 0}})

      assert %Parser{global: [function], local: nil} = context
      assert %Nif{name: :foo, arity: 0, params: []} = function
    end

    test "correctly obtains one parameters" do
      assert {:ok, _, _, context, _, _} = Parser.parse_function_header("""
        fn foo(bar: i64) i64 {
      """, context: %{local: %Nif{name: :foo, arity: 1}})

      assert %Parser{global: [function], local: nil} = context
      assert %Nif{name: :foo, arity: 1, params: ["i64"]} = function
    end

    test "raises compile error if the names mismatch" do
      assert_raise CompileError, fn -> Parser.parse_function_header("""
          fn bar() i64 {
        """, context: %{local: %Nif{name: :foo, arity: 0}})
      end
    end

    test "raises compile error if the arities mismatch" do
      assert_raise CompileError, fn -> Parser.parse_function_header("""
          fn foo() i64 {
        """, context: %{local: %Nif{name: :foo, arity: 1}})
      end
    end

    test "raises compile error if the arities mismatch, with a beam.env parameter" do
      assert_raise CompileError, fn -> Parser.parse_function_header("""
          fn foo(env: beam.env) i64 {
        """, context: %{local: %Nif{name: :foo, arity: 1}})
      end
    end

    test "raises compile error if the arities mismatch, with a ErlNifEnv parameter" do
      assert_raise CompileError, fn -> Parser.parse_function_header("""
          fn foo(env: ?*e.ErlNifEnv) i64 {
        """, context: %{local: %Nif{name: :foo, arity: 1}})
      end
    end

    test "raises compile error on an invalid return type" do
      # but not if we don't have preloaded nif value
      assert Parser.parse_function_header("""
        fn foo() !i64 {
      """)

      assert_raise CompileError, fn -> Parser.parse_function_header("""
          fn foo() !i64 {
        """, context: %{local: %Nif{name: :foo, arity: 0}})
      end
    end

    test "raises compile error on an invalid parameter type" do
      # but not if we don't have preloaded nif value
      assert Parser.parse_function_header("""
        fn foo(bar: strange.type) i64 {
      """)

      assert_raise CompileError, fn ->
        Parser.parse_function_header("""
          fn foo(bar: strange.type) i64 {
        """, context: %{local: %Nif{name: :foo, arity: 1}})
      end
    end
  end
end
