defmodule ZiglerTest.CornerCases.ReleaseMode do
  use ExUnit.Case, async: true

  release_mode = :debug

  use Zig, otp_app: :zigler, release_mode: release_mode

  ~Z"""
  pub fn add(a: u32, b: u32) u32 {
      return a + b;
  }
  """
end