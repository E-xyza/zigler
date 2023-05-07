defmodule ZiglerTest.Types.FloatTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    leak_check: true,
    otp_app: :zigler

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

    test "argument error if integers are sent" do
      # note that integers are wrongly typed.  You must explicitly send floats.
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     got: `3`\n     note: integers are not allowed as arguments to float\n",
                   fn -> f64_div2(3) end
    end

    test "argument error if non-special atoms are sent" do
      # only certain atoms are allowed for float
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     got: `:foo`\n     note: not an atom value for f64 (should be one of `[:infinity, :neg_infinity, :NaN]`\n",
                   fn -> f64_div2(:foo) end
    end

    test "argument error if other incorrect data are sent" do
      # only certain atoms are allowed for float
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     got: `\"foo\"`\n",
                   fn -> f64_div2("foo") end
    end
  end
end
