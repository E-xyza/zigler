defmodule ZigTest.BadParameterType do
  use Zigler, app: :zigler

  ~Z"""
  /// nif: bad_param/1
  fn bad_param(x: u64) i64 {
    return @intCast(i64, x);
  }
  """
end
