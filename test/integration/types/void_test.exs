defmodule ZiglerTest.Types.VoidTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn void_test() void {}
  """

  describe "for a basic void" do
    test "it returns `:ok`" do
      assert :ok == void_test()
    end
  end
end
