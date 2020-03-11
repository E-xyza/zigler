defmodule ZigTest.BadArgumentType do
  use Zigler, otp_app: :zigler

  ~Z"""
  /// nif: bad_retval/1
  fn bad_retval(x: i64) u64 {
    return @intCast(u64, x);
  }
  """
end
