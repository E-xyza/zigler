defmodule ZiglerTest.PackageTest do
  use ExUnit.Case, async: true

  test "includes Windows NIF compatibility headers" do
    files = Mix.Project.config() |> Keyword.fetch!(:package) |> Keyword.fetch!(:files)

    assert "priv/erl_nif_win" in files
    assert File.regular?("priv/erl_nif_win/erl_nif_win.h")
    assert File.regular?("priv/erl_nif_win/erl_nif_api_funcs_win.h")
  end
end
