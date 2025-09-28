defmodule ZiglerTest.EmbeddedSemaTest do
  require Logger

  alias Zig.Sema

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
      |> Path.join("#{__MODULE__}.so")

    {_, sema_text} = Sema._obtain_precompiled_sema_json(%{precompiled: file})
    assert %{"functions" => [%{"name" => "add_one"}]} = JSON.decode!(sema_text)
  end
end
