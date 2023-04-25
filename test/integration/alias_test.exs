defmodule ZiglerTest.AliasTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      ok: [alias: true],
      renamed: [alias: :ok]
    ]

  ~Z"""
  const beam = @import("beam");

  pub fn ok() void { }
  """

  test "intermediate content actually has the aliased form"

  test "aliased call" do
    assert :ok = ok()
  end

  test "renamed call" do
    assert :ok = renamed()
  end
end
