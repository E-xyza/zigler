defmodule ZiglerTest.Make.ElixirStructTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    import: [structs: [NaiveDateTime]]
    local_zig: true

  ~Z"""
  const beam = @import("beam");

  pub fn make_naive_datetime_tuple(env: beam.env) beam.term {
    return beam.make(env, NaiveDateTime{.year = 2022, .month = 8, .day = 5, .hour = 12, .minute = 0, .second = 0});
  }
  """

  test "an :ok tuple can be returned" do
    assert {:ok, 47} = make_ok_tuple()
  end
end
