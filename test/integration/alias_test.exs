defmodule ZiglerTest.AliasTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :skip
  test "restore"

#  use Zig,
#    otp_app: :zigler,
#    nifs: [
#      ...,
#      renamed: [alias: :ok]
#    ]
#
#  ~Z"""
#  const beam = @import("beam");
#
#  pub fn ok() void { }
#  """
#
#  test "intermediate content actually has the aliased form" do
#    assert __DIR__
#           |> Path.join(".#{__MODULE__}.zig")
#           |> File.read!()
#           |> Kernel.=~("pub const renamed = ok;")
#  end
#
#  test "aliased call" do
#    assert :ok = ok()
#  end
#
#  test "renamed call" do
#    assert :ok = renamed()
#  end
end
