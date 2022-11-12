defmodule ZiglerTest.Types.FloatTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn f64_div2(value: f64) f64 { return value / 2; }

  pub fn f32_div2(value: f32) f32 { return value / 2; }

  pub fn f16_div2(value: f16) f16 { return value / 2; }
  """

  describe "given an fp function" do
    test "you can work with f64" do
      assert 1.5 = f64_div2(3.0)
    end

    test "you can work with f32" do
      assert 1.5 = f32_div2(3.0)
    end

    test "you can work with f16" do
      assert 1.5 = f16_div2(3.0)
    end

    test "you can send infinity to f64" do
      assert :infinity = f64_div2(:infinity)
      assert :neg_infinity = f64_div2(:neg_infinity)
    end

    test "you can send infinity to f32" do
      assert :infinity = f32_div2(:infinity)
      assert :neg_infinity = f32_div2(:neg_infinity)
    end

    test "you can send infinity to f16" do
      assert :infinity = f16_div2(:infinity)
      assert :neg_infinity = f16_div2(:neg_infinity)
    end

    test "you can send NaN to f64" do
      assert :NaN = f64_div2(:NaN)
    end

    test "you can send NaN to f32" do
      assert :NaN = f32_div2(:NaN)
    end

    test "you can send NaN to f16" do
      assert :NaN = f16_div2(:NaN)
    end
  end
end
