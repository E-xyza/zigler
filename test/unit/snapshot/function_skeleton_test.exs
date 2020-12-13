defmodule ZiglerTest.Snapshot.FunctionSkeletonTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser.Nif

  @moduletag :snapshot

  test "an arity zero function is produced correctly" do

    result = quote context: Elixir do
      @spec foo() :: nil
      def foo do
        raise "nif for function foo/0 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(%Nif{name: :foo, arity: 0}) == result
  end

  test "an arity one function is produced correctly" do

    result = quote context: Elixir do
      @spec foo() :: nil
      def foo(_) do
        raise "nif for function foo/1 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(%Nif{name: :foo, arity: 1}) == result
  end

  test "a zero-arity threaded function is produced correctly" do

    result = quote context: Elixir do
      @spec foo() :: nil
      def foo do
        {:ok, ref} = __foo_launch__()
        try do
          receive do
            {^ref, return} ->
              return
            {:error, :enomem} ->
              raise("no memory")
            {:error, :function_clause} ->
              raise(%FunctionClauseError{module: __MODULE__, function: :foo, arity: 0})
          end
        rescue
          error ->
            reraise(error, __STACKTRACE__)
        after
          __foo_cleanup__(ref)
        end
      end

      def __foo_launch__ do
        raise "nif launcher for function foo/0 not bound"
      end

      def __foo_cleanup__(_) do
        raise "nif cleanup for function foo/0 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(
      %Nif{name: :foo, arity: 0, opts: [concurrency: :threaded]}) == result
  end

  test "a one-arity threaded function is produced correctly" do

    result = quote context: Elixir do
      @spec foo(integer) :: nil
      def foo(arg1) do
        {:ok, ref} = __foo_launch__(arg1)
        try do
          receive do
            {^ref, return} ->
              return
            {:error, :enomem} ->
              raise("no memory")
            {:error, :function_clause} ->
              raise(%FunctionClauseError{module: __MODULE__, function: :foo, arity: 1})
          end
        rescue
          error ->
            reraise(error, __STACKTRACE__)
        after
          __foo_cleanup__(ref)
        end
      end

      def __foo_launch__(_) do
        raise "nif launcher for function foo/1 not bound"
      end

      def __foo_cleanup__(_) do
        raise "nif cleanup for function foo/1 not bound"
      end
    end

    assert Zigler.Compiler.function_skeleton(
      %Nif{name: :foo, arity: 1, opts: [concurrency: :threaded], args: ["i64"]}) == result
  end
end
