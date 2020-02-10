defmodule ZigSyntaxBadIncludeModule do
  use Zigler, app: :zigler

  ~Z"""
  pub const add = @import("bad.zig");

  /// nif: one/0
  fn one() i64 {
    return add.one;
  }
  """
end
