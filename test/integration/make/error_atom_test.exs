defmodule ZiglerTest.Make.ErrorAtomTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn test_error_atom(env: beam.env) beam.term {
    return beam.make_error_atom(env);
  }

  pub fn test_error_pair(env: beam.env) beam.term {
    return beam.make_error_pair(env, .some_error, .{});
  }
  """

  test "error atom" do
    assert :error = test_error_atom()
  end

  test "error pair" do
    assert {:error, :some_error} = test_error_pair()
  end
end
