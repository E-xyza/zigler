defmodule ZiglerTest.Make.TupleTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    local_zig: true

  ~Z"""
  const beam = @import("beam");

  pub fn make_ok_tuple(env: beam.env) beam.term {
    return beam.make(env, .{.ok, 47});
  }
  """

  test "an :ok tuple can be returned" do
    assert {:ok, 47} = make_ok_tuple()
  end
end
