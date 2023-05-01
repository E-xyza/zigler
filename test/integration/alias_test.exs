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

  test "intermediate content actually has the aliased form" do
    assert __DIR__
<<<<<<< HEAD
    |> Path.join(".#{__MODULE__}.zig")
    |> File.read!
    |> Kernel.=~("pub const renamed = ok;")
=======
           |> Path.join(".#{__MODULE__}.zig")
           |> File.read!()
           |> Kernel.=~("pub const renamed = ok;")
>>>>>>> e87e60ed73e62554afb1a7cd13b0579e265d69f8
  end

  test "aliased call" do
    assert :ok = ok()
  end

  test "renamed call" do
    assert :ok = renamed()
  end
end
