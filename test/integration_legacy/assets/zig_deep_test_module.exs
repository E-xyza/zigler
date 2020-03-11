defmodule ZiglerTest.ZigDeepTestModule do
  use Zigler, otp_app: :zigler

  ~Z"""
  const my_test = @import("test.zig");

  /// nif: zeroarity/0
  fn zeroarity() i64 {
    return 47;
  }
  """
end
