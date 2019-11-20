defmodule ZigSyntaxErrorModule do
  use Zigler, app: :zigler

  ~Z"""
  /// nif: two/0
  fn two() i64 {
    return 2;
  }
  """

  ~Z"""
  /// nif: one/0
  fn one() i64 {
    return 1  // <-- syntax error here, line 14.
  }
  """
end
