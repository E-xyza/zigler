defmodule ZiglerTest.MultiplatformPrecompiledTest do
  use Zig, otp_app: :zigler
  ~Z"""
  pub fn add_one(x: u32) u32 {
    return x + 1;
  }
  """
end
