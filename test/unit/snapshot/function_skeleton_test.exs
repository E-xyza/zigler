defmodule ZiglerTest.Snapshot.FunctionSkeletonTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser.Nif

  test "an arity zero function is produced correctly" do

    result = quote context: Elixir do
      def foo do
        raise "nif for function foo/0 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(%Nif{name: :foo, arity: 0}) == result
  end

  test "an arity one function is produced correctly" do

    result = quote context: Elixir do
      def foo(_) do
        raise "nif for function foo/1 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(%Nif{name: :foo, arity: 1}) == result
  end

  test "a zero-arity long-running function is produced correctly" do

    result = quote context: Elixir do
      def foo() do
        resource = __foo_launch__()
        receive do :finished -> :ok end
        __foo_fetch__(resource)
      end

      def __foo_launch__() do
        raise "nif launcher for function foo/0 not bound"
      end

      def __foo_fetch__(_) do
        raise "nif fetcher for function foo/0 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(
      %Nif{name: :foo, arity: 0, opts: [long: true]}) == result
  end

  test "a one-arity long-running function is produced correctly" do

    result = quote context: Elixir do
      def foo(arg1) do
        resource = __foo_launch__(arg1)
        receive do :finished -> :ok end
        __foo_fetch__(resource)
      end

      def __foo_launch__(_) do
        raise "nif launcher for function foo/1 not bound"
      end

      def __foo_fetch__(_) do
        raise "nif fetcher for function foo/1 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(
      %Nif{name: :foo, arity: 1, opts: [long: true]}) == result
  end

end
