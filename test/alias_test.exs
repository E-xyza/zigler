defmodule ZiglerTest.AliasTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      ...,
      renamed: [alias: :ok]
    ]

  ~Z"""
  const beam = @import("beam");

  pub fn ok() void { }
  """

  test "aliased call" do
    assert :ok = ok()
  end

  test "renamed call" do
    assert :ok = renamed()
  end
end
