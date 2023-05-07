defmodule ZiglerTest.Erlang.QuoteErlTest do
  use ExUnit.Case, async: true
  import Zig.QuoteErl

  describe "quoteerl can" do
    test "generate a module attribute" do
      assert [{:attribute, _, :module, :foo}] =
               quote_erl("""
               -module(foo).
               """)
    end

    test "generate a function" do
      assert [{:function, _, :foo, 1, [{:clause, _, [{:var, _, :_X}], _, [{:atom, _, :foo}]}]}] =
               quote_erl("""
               foo(_X) ->
                 foo.
               """)
    end

    test "substitute into a function name" do
      assert [{:function, _, :bar, 1, [{:clause, _, [{:var, _, :_X}], _, [{:atom, _, :foo}]}]}] =
               quote_erl(
                 """
                 unquote(foo)(_X) ->
                   foo.
                 """,
                 foo: :bar
               )
    end

    test "substitute into an atom literal" do
      assert [{:function, _, :foo, 0, [{:clause, _, [], _, [{:atom, _, :bar}]}]}] =
               quote_erl(
                 """
                 foo() ->
                   unquote(foo).
                 """,
                 foo: :bar
               )
    end

    test "substitute as a string" do
      assert [
               {:function, _, :foo, 0,
                [{:clause, _, [], _, [{:string, _, ~C'my favorite string'}]}]}
             ] =
               quote_erl(
                 """
                 foo() ->
                   unquote(string).
                 """,
                 string: ~C(my favorite string)
               )
    end

    test "substitute vars" do
      assert [{:function, _, :foo, 1, [{:clause, _, [{:var, _, :_X}], _, [{:atom, _, :foo}]}]}] =
               quote_erl(
                 """
                 foo(unquote(var)) ->
                   foo.
                 """,
                 var: {:var, :_X}
               )
    end

    test "substitute multiple things" do
      assert [
               {:function, _, :foo, 2,
                [{:clause, _, [{:atom, _, :foo}, {:var, _, :_X}], _, [{:atom, _, :foo}]}]}
             ] =
               quote_erl(
                 """
                 foo(unquote(...vars)) ->
                   foo.
                 """,
                 vars: [:foo, var: :_X]
               )
    end
  end
end
