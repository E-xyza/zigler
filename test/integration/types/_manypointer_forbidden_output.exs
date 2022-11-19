defmodule ZiglerTest.Types.ManypointerForbiddenOutput do
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn forbidden() [*]u8 {
    return @intToPtr([*]u8, 1);
  }
  """
end
