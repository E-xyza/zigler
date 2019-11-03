defmodule ZigSyntaxErrorModule do
  use Zigler, app: :zigler

  ~Z"""
  /// nif: oops/0
  fn two() i64 {
    return 2;
  }
  """
end
