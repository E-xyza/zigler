defmodule ZiglerTest.SigilZ.OneTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn nif1() u16 { return 47; }
  """

  setup do
    {:ok, code: Zig.code(__MODULE__)}
  end

  test "the function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:5\npub fn nif1()"
  end
end
