defmodule ZiglerTest.AliasedTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [ok: [alias: true]]

  ~Z"""
  const beam = @import("beam");

  pub fn ok(env: beam.env) beam.term { return beam.make(env, .ok, .{}); }
  """

  test "intermediate content actually has the aliased form"

  test "aliased call" do
    assert :ok = ok()
  end
end
