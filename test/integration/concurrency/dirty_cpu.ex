defmodule ZiglerTest.Concurrency.DirtyCpu do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler, nifs: [dirty_cpu: [:dirty_cpu]]

  ~Z"""
  pub fn dirty_cpu() void {}
  """

  test "dirty_cpu tagged function" do
    assert :ok = dirty_cpu()
  end
end
