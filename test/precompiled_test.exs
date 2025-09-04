defmodule ZiglerTest.Compiled do
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn add_one(x: u32) u32 {
      return x + 1;
  }
  """
end

defmodule ZiglerTest.PrecompiledTest do
  use ExUnit.Case, async: true

  require ZiglerTest.Compiled

  use Zig, otp_app: :zigler, precompiled: "./priv/"
end
