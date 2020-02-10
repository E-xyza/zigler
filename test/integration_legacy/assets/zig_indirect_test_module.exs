defmodule ZiglerTest.ZigIndirectTestModule do
  use Zigler, app: :zigler

  ~Z"""
  const my_test = @import("indirect_test.zig");

  /// nif: zeroarity/0
  fn zeroarity() i64 {
    return 47;
  }
  """
end
