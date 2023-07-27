defmodule ZiglerTest.Concurrency.DirtyIoTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, nifs: [dirty_io: [:dirty_io]]

  ~Z"""
  pub fn dirty_io() void {}
  """

  test "dirty_io tagged function" do
    assert :ok = dirty_io()
  end
end
