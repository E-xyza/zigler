defmodule ZiglerTest.SigilZ.MultiTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn first() u16 { return 47; }
  """

  ~Z"""
  pub fn next() u16 { return 47; }
  """

  ~Z"""
  pub fn last() u16 { return 47; }
  """

  setup do
    {:ok, code: Zig.code(__MODULE__)}
  end

  test "first function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:5\npub fn first()"
  end

  test "next function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:9\npub fn next()"
  end

  test "last function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:13\npub fn last()"
  end
end
