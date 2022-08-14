defmodule ZiglerTest.Code.OneSigilZTest do
  use ExUnit.Case, async: true
  use Zig, compile: false, nifs: [:nif]

  ~Z"""
  fn nif() u16 { return 47; }
  """

  setup do
    {:ok, code: Zig.code(__MODULE__)}
  end

  test "the function is branded with the correct file and line number", %{code: code} do
    file = Path.relative_to_cwd(__ENV__.file)
    assert code =~ "#{file}:5\nfn nif()"
  end
end
