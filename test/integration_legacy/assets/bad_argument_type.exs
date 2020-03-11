defmodule ZigTest.BadArgumentType do
  use Zigler, otp_app: :zigler

  ~Z"""
  /// nif: bad_arg/1
  fn bad_arg(x: u64) i64 {
    return @intCast(i64, x);
  }
  """
end
