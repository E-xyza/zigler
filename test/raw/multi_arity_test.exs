defmodule ZiglerTest.Raw.MultiArityTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :raw

  use Zig,
    otp_app: :zigler,
    nifs: [
      range_arity: [arity: 1..3],
      list_arity: [arity: [1, 3..4]]
    ]

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn range_arity(env: beam.env, arity: c_int, params: [*]const beam.term) beam.term {
      return beam.make(params[0..@intCast(arity)], .{ .env = env });
  }

  pub fn list_arity(env: beam.env, arity: c_int, params: [*]const beam.term) beam.term {
      return beam.make(params[0..@intCast(arity)], .{ .env = env });
  }
  """

  test "raw call with range arity" do
    for arity <- 1..3 do
      foos = List.duplicate(:foo, arity)
      assert foos == apply(__MODULE__, :range_arity, foos)
    end

    assert_raise UndefinedFunctionError, fn ->
      apply(__MODULE__, :range_arity, [])
    end

    assert_raise UndefinedFunctionError, fn ->
      apply(__MODULE__, :range_arity, [:foo, :foo, :foo, :foo])
    end
  end

  test "raw call with list of arity mixtures" do
    for arity <- 3..4 do
      assert List.duplicate(:foo, arity) ==
               apply(__MODULE__, :list_arity, List.duplicate(:foo, arity))
    end

    assert_raise UndefinedFunctionError, fn ->
      apply(__MODULE__, :list_arity, [])
    end

    assert_raise UndefinedFunctionError, fn ->
      apply(__MODULE__, :list_arity, [:foo, :foo])
    end

    assert_raise UndefinedFunctionError, fn ->
      apply(__MODULE__, :list_arity, [:foo, :foo, :foo, :foo, :foo])
    end
  end
end
