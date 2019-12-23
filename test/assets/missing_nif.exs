defmodule ZigTest.MissingNif do
  use Zigler, app: :zigler

  ~Z"""
  /// nif: missing_nif/1
  // just a comment here
  """
end
