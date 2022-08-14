defmodule ZiglerTest.Sema.FunctionTest do
  use ExUnit.Case, async: true

  alias Zig.Type
  alias Zig.Sema

  import Type, only: :macros

  defp random do
    16
    |> :crypto.strong_rand_bytes()
    |> Base.encode16()
  end

  defp build(content) do
    path = Path.join([System.tmp_dir!(), "zig-test-sema-" <> random()])
    File.mkdir_p!(path)

    priv_dir =
      :zigler
      |> :code.priv_dir()
      |> Path.join("beam")

    for file <- ~w"beam_mutex.zig beam.zig erl_nif.zig" do
      priv_dir
      |> Path.join(file)
      |> File.cp!(Path.join(path, file))
    end

    zig_file = Path.join(path, "basic.zig")
    File.write!(zig_file, content)
    zig_file
  end

  # basic test on semantic analysis
  @basic """
  pub fn basic(x: u8) u8 {
    return x + 1;
  }
  """

  test "a basic function can be found" do
    basic = build(@basic)

    assert [
             %Type.Function{
               name: :basic,
               arity: 1,
               params: [~t(u8)],
               return: ~t(u8)
             }
           ] = Sema.analyze_file!(basic, nifs: [:basic])
  end

  @multiple_params """
  pub fn multiple_params(x: u8, y: u16) u16 {
    return x + y;
  }
  """

  test "a function with multiple types" do
    multiple_params = build(@multiple_params)

    assert [
             %Type.Function{
               name: :multiple_params,
               arity: 2,
               params: [~t(u8), ~t(u16)],
               return: ~t(u16)
             }
           ] = Sema.analyze_file!(multiple_params, nifs: [:multiple_params])
  end

  @beam_env """
  const beam = @import("beam.zig");

  pub fn beam_env(env: beam.env, x: u8) u8 {
    _ = env;
    return x;
  }
  """

  test "a function with beam.env" do
    beam_env = build(@beam_env)

    assert [
             %Type.Function{
               name: :beam_env,
               arity: 1,
               params: [~t(u8)],
               return: ~t(u8)
             }
           ] = Sema.analyze_file!(beam_env, nifs: [:beam_env])
  end

  @erl_nif_env """
  const beam = @import("beam.zig");

  pub fn erl_nif_env(env: beam.env, x: u8) u8 {
    _ = env;
    return x;
  }
  """

  test "a function with e.ErlNifEnv" do
    erl_nif_env = build(@erl_nif_env)

    assert [
             %Type.Function{
               name: :erl_nif_env,
               arity: 1,
               params: [~t(u8)],
               return: ~t(u8)
             }
           ] = Sema.analyze_file!(erl_nif_env, nifs: [:erl_nif_env])
  end
end
