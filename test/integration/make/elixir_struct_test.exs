defmodule ZiglerTest.Make.ElixirStructTest do
  use ExUnit.Case, async: true

  @moduletag :skip

  #  use Zig,   #    otp_app: :zigler,
  #    import: [structs: [NaiveDateTime]]
  #
  #  ~Z"""
  #  const beam = @import("beam");
  #
  #  pub fn make_naive_datetime_struct(env: beam.env) beam.term {
  #    return beam.make(env, NaiveDateTime{.year = 2022, .month = 8, .day = 5, .hour = 12, .minute = 0, .second = 0});
  #  }
  #  """
  #
  #  test "an :ok tuple can be returned" do
  #    assert %NaiveDateTime{year: 2022, month: 8, day: 5, hour: 12, minute: 0, second: 0} ==
  #             make_naive_datetime_struct()
  #  end
end
