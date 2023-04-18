defmodule ZiglerTest.QuoteErlTest do
  use ExUnit.Case, async: true
  import Zig.QuoteErl

  test "quoteerl can generate a module attribute" do
    assert [{:attribute, _, :module, :foo}] =
             quote_erl("""
             -module(foo).
             """)
  end

  test "quoteerl can generate a function" do
    assert [{:function, _, :foo, 1, [{:clause, _, [{:var, _, :_X}], _, [{:atom, _, :foo}]}]}] =
             quote_erl("""
             foo(_X) ->
               foo.
             """)
  end

  test "quoteerl can substitute into a function name" do
    assert [{:function, _, :bar, 1, [{:clause, _, [{:var, _, :_X}], _, [{:atom, _, :foo}]}]}] =
             quote_erl(
               """
               unquote(foo)(_X) ->
                 foo.
               """,
               foo: :bar
             )
  end

  test "quoteerl can substitute into an atom literal" do
    assert [{:function, _, :foo, 0, [{:clause, _, [], _, [{:atom, _, :bar}]}]}] =
             quote_erl(
               """
               foo() ->
                 unquote(foo).
               """,
               foo: :bar
             )
  end

  test "quoteerl can substitute as a string" do
    assert [{:function, _, :foo, 0, [{:clause, _, [], _, [{:string, 2, ~C'my favorite string'}]}]}] =
             quote_erl(
               """
               foo() ->
                 unquote(string).
               """,
               string: ~C(my favorite string)
             )
  end
end
