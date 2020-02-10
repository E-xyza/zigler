defmodule ZiglerTest.Snapshot.FunctionSkeletonTest do
  use ExUnit.Case, async: true

  test "an arity zero function is produced correctly" do

    result = quote context: Elixir do
      def foo() do
        raise "nif for function foo/0 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(%{name: :foo, arity: 0}) == result
  end

  test "an arity one function is produced correctly" do

    result = quote context: Elixir do
      def foo(_) do
        raise "nif for function foo/1 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(%{name: :foo, arity: 1}) == result
  end

end
