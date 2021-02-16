defmodule ZiglerTest.Snapshot.FunctionSkeletonTest do
  use ExUnit.Case, async: true

  alias Zig.Parser.Nif

  @moduletag :snapshot

  test "an arity zero function is produced correctly" do

    result = quote context: Elixir do
      @spec foo() :: nil
      def foo do
        raise "nif for function foo/0 not bound"
      end
    end

    assert Zig.Compiler.function_skeleton(%Nif{name: :foo, arity: 0}) == result
  end

  test "an arity one function is produced correctly" do

    result = quote context: Elixir do
      @spec foo() :: nil
      def foo(_) do
        raise "nif for function foo/1 not bound"
      end
    end

    assert Zig.Compiler.function_skeleton(%Nif{name: :foo, arity: 1}) == result
  end

  test "a zero-arity threaded function is produced correctly" do

    result = quote context: Elixir do
      @spec foo() :: nil
      def foo do
        case __foo_launch__() do
          {:ok, ref} ->
            receive do
              {:ok, {^ref, return}} ->
                __foo_cleanup__(ref)
                return
              {:error, {^ref, :enomem}} ->
                __foo_cleanup__(ref)
                raise "no memory"
              {:error, {^ref, :function_clause}} ->
                __foo_cleanup__(ref)
                raise %FunctionClauseError{
                  module: __MODULE__,
                  function: :foo,
                  arity: 0
                }
              {:error, :thread_resource_error} ->
                raise "thread resource error for #{__ENV__.function}"
            end
          {:error, error} ->
            raise error
        end
      end

      def __foo_launch__ do
        raise "nif launcher for function foo/0 not bound"
      end

      def __foo_cleanup__(_) do
        raise "nif cleanup for function foo/0 not bound"
      end
    end

    assert Zig.Compiler.function_skeleton(
      %Nif{name: :foo, arity: 0, opts: [concurrency: :threaded]}) == result
  end

  test "a one-arity threaded function is produced correctly" do

    result = quote context: Elixir do
      @spec foo(integer) :: nil
      def foo(arg1) do
        case __foo_launch__(arg1) do
          {:ok, ref} ->
            receive do
              {:ok, {^ref, return}} ->
                __foo_cleanup__(ref)
                return
              {:error, {^ref, :enomem}} ->
                __foo_cleanup__(ref)
                raise "no memory"
              {:error, {^ref, :function_clause}} ->
                __foo_cleanup__(ref)
                raise %FunctionClauseError{
                  module: __MODULE__,
                  function: :foo,
                  arity: 1
                }
              {:error, :thread_resource_error} ->
                raise "thread resource error for #{__ENV__.function}"
            end
          {:error, error} ->
            raise error
        end
      end

      def __foo_launch__(_) do
        raise "nif launcher for function foo/1 not bound"
      end

      def __foo_cleanup__(_) do
        raise "nif cleanup for function foo/1 not bound"
      end
    end

    assert Zig.Compiler.function_skeleton(
      %Nif{name: :foo, arity: 1, opts: [concurrency: :threaded], args: ["i64"]}) == result
  end
end
