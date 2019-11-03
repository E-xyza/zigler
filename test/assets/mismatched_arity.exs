defmodule ZigSyntaxErrorModule do
  use Zigler, app: :zigler

  ~Z"""
  /// nif: oops/2
  fn oops() i64 {
    return 2;
  }
  """
end
