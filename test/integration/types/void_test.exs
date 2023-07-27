defmodule ZiglerTest.Types.VoidTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    leak_check: true,
    otp_app: :zigler

  ~Z"""
  pub fn void_test() void {}
  """

  describe "for a basic void" do
    test "it returns `:ok`" do
      assert :ok == void_test()
    end
  end
end
