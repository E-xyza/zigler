defmodule ZiglerTest.Sema.FunctionTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, precompile: false

  alias Zig.Sema
  alias Zig.Type

  import Type, only: :macros

  setup_all do
    sema_result =
      __DIR__
      |> Path.join(".#{__MODULE__}.zig")
      |> Sema.run_sema!(__MODULE__)
      |> Map.get(:functions)
      |> Map.new(&{&1.name, &1})

    {:ok, sema_result}
  end

  # basic test on semantic analysis
  ~Z"""
  pub fn basic(x: u8) u8 {
    return x + 1;
  }
  """

  test "a basic function can be found", %{basic: basic} do
    assert %Type.Function{
             name: :basic,
             arity: 1,
             params: [~t(u8)],
             return: ~t(u8)
           } = basic
  end

  ~Z"""
  pub fn multiple_params(x: u8, y: u16) u16 {
    return x + y;
  }
  """

  test "a function with multiple types", %{multiple_params: multiple_params} do
    assert %Type.Function{
             name: :multiple_params,
             arity: 2,
             params: [~t(u8), ~t(u16)],
             return: ~t(u16)
           } = multiple_params
  end

  ~Z"""
  const beam = @import("beam");

  pub fn beam_env(env: beam.env, x: u8) u8 {
    _ = env;
    return x;
  }
  """

  test "a function with beam.env", %{beam_env: beam_env} do
    assert %Type.Function{
             name: :beam_env,
             arity: 1,
             params: [:env, ~t(u8)],
             return: ~t(u8)
           } = beam_env
  end

  ~Z"""
  const e = @import("erl_nif");

  pub fn erl_nif_env(env: ?*e.ErlNifEnv, x: u8) u8 {
    _ = env;
    return x;
  }
  """

  test "a function with e.ErlNifEnv", %{erl_nif_env: erl_nif_env} do
    assert %Type.Function{
             name: :erl_nif_env,
             arity: 1,
             params: [:env, ~t(u8)],
             return: ~t(u8)
           } = erl_nif_env
  end
end
