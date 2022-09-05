defmodule ZiglerTest.Types.BoolTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    local_zig: true

  ~Z"""
  pub fn bool_test(b: bool) bool {
    return !b;
  }
  """

  describe "for a bool" do
    test "you can pass erlang booleans" do
      assert bool_test(false)
      refute bool_test(true)
    end

    test "non boolean types are not allowed" do
      assert_raise ArgumentError, "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: boolean\n     got: 1\n", fn -> bool_test(1) end
    end
  end
end
