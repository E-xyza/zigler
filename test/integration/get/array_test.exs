defmodule ZiglerTest.Get.ArrayTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");
  const std = @import("std");

  pub fn array_beam_term_get_test(env: beam.env, passed: beam.term) f64 {
    const buf = beam.get([3]beam.term, env, passed) catch unreachable;
    var sum: f64 = 0.0;
    for (buf) |val| { sum += beam.get(f64, env, val) catch unreachable; }
    return sum;
  }
  """

  describe "beam.get for" do
    test "beam.term array works" do
      assert 6.0 = array_beam_term_get_test([1.0, 2.0, 3.0])
    end
  end
end
