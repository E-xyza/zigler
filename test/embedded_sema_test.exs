defmodule ZiglerTest.EmbeddedSemaTest do
  require Logger

  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn add_one(x: u32) u32 {
      return x + 1;
  }
  """

  test "linksection" do
    file =
      :zigler
      |> :code.priv_dir()
      |> Path.join("lib")
      |> Path.join("lib#{__MODULE__}.so")

    {sema, 0} = System.cmd("objcopy", ["--dump-section", ".sema=/dev/stdout", file])

    assert %{"functions" => [%{"name" => "add_one"}]} =
             sema
             |> String.trim(<<0>>)
             |> Zig.json_decode!()
  end
end
