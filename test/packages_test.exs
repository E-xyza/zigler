defmodule ZiglerTest.PackagesTest do
  use ZiglerTest.IntegrationCase

  require Logger

  use Zig,
    otp_app: :zigler,
    packages: [extra: {"test/_support/package/extra.zig", [:beam]}]

  ~Z"""
  const extra = @import("extra");

  pub fn extra_value() u64 {
      return extra.value;
  }
  """

  test "package file" do
    assert 47 = extra_value()
  end
end
