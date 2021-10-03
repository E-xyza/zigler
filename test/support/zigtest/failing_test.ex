defmodule ZiglerTest.ZigTest.FailingTest do
  @moduledoc false

  use Zig

  ~Z"""
  /// nif: one/0
  fn one() i64 {
    return 1;
  }

  test "a lie" {
    try std.testing.expect(one() == 2);
  }

  test "a multiline lie" {
    try std.testing.expect(
      one() == 2
    );
  }

  test "a truth and a lie" {
    try std.testing.expect(1 == 1);
    try std.testing.expect(
      one() == 2
    );
  }

  const UnrelatedError = error { Unrelated };

  fn unrelated() !void {
    return UnrelatedError.Unrelated;
  }

  test "an unrelated error" {
    try unrelated();
  }
  """
end
