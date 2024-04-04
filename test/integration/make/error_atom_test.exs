defmodule ZiglerTest.Make.ErrorAtomTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn test_error_atom() beam.term {
    return beam.make_error_atom(.{});
  }

  pub fn test_error_pair() beam.term {
    return beam.make_error_pair(.some_error, .{});
  }
  """

  test "error atom" do
    assert :error = test_error_atom()
  end

  test "error pair" do
    assert {:error, :some_error} = test_error_pair()
  end
end
