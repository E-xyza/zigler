defmodule ZiglerTest.EmbeddedSema do
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn add_one(x: u32) u32 {
      return x + 1;
  }
  """
end

defmodule ZiglerTest.EmbeddedSemaTest do
  require Logger

  alias Zig.Sema

  use ExUnit.Case, async: true

  test "linksection" do
    # this must be purged otherwise if a memory-mapped, dlloaded so file is read
    # by different mechanism a linux gets really upset and causes a segfault.
    :code.purge(ZiglerTest.EmbeddedSema)

    file =
      :zigler
      |> :code.priv_dir()
      |> Path.join("lib")
      |> Path.join("ZiglerTest.EmbeddedSema.so")

    {_, sema_text} = Sema._obtain_precompiled_sema_json(%{precompiled: file})
    assert %{"functions" => [%{"name" => "add_one"}]} = JSON.decode!(sema_text)
  end
end
