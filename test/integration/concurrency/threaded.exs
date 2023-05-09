defmodule ZiglerTest.Concurrency.ThreadedTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, nifs: [threaded: [:threaded]]

  ~Z"""
  pub fn threaded() void {}
  """

  test "threaded function" do
    assert :ok = threaded()
  end
end
