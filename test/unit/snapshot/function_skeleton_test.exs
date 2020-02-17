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

  test "a zero-arity long-running function is produced correctly" do

    result = quote context: Elixir do
      def foo() do
        resource = __foo_launcher__()
        receive do :finished -> :ok end
        __foo_fetcher__(resource)
      end

      def __foo__launcher__() do
        raise "launcher for function foo/1 not bound"
      end

      def __foo_fetcher__() do
        raise "fetcher for function foo/1 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(
      %{name: :foo, arity: 0, opts: [long: true]}) == result
  end

end
