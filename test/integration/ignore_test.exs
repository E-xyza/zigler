defmodule ZiglerTest.IgnoreTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    ignore: [:ignored]

  ~Z"""
  const beam = @import("beam");

  pub fn ignored() void {}

  pub fn present() void {}
  """

  test "non-ignored call" do
    assert function_exported?(__MODULE__, :present, 0)
  end

  test "ignored call" do
    refute function_exported?(__MODULE__, :ignored, 0)
  end
end
