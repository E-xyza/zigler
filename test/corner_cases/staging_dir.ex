defmodule ZiglerTest.StagingDir do
  @moduledoc false

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn add_one(number: u32) u32 {
    return number + 1;
  }
  """
end
