defmodule ZiglerTest.Types.Errors.ManypointerReturnFails do
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn forbidden() [*]u8 {
    return @ptrFromInt(1);
  }
  """
end
