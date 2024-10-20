defmodule ZiglerTest.CornerCases.ReleaseMode do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler, release_mode: :safe

  ~Z"""
  pub fn add(a: u32, b: u32) u32 {
      return a + b;
  }
  """

  test "release mode safe works" do
    assert 48 = add(47, 1)
  end
end