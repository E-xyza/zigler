defmodule ZiglerTest.SigilZ.MultiTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler, precompile: false

  ~Z"""
  fn first() u16 { return 47; }
  """

  ~Z"""
  fn next() u16 { return 47; }
  """

  ~Z"""
  fn last() u16 { return 47; }
  """

  setup do
    {:ok, code: Zig.code(__MODULE__)}
  end

  test "first function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:5\nfn first()"
  end

  test "next function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:9\nfn next()"
  end

  test "last function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:13\nfn last()"
  end
end
