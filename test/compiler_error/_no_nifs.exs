defmodule ZiglerTest.CompilerError.NoNifs do
  use Zig, otp_app: :zigler

  ~Z"""
  fn oops_not_pub() u32 {
      return 42;
  }
  """
end
