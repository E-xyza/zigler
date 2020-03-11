
defmodule AllTheDocs do
  use Zigler, otp_app: :zigler
  ~Z"""
  /// a zero-arity function which returns 47.
  /// nif: zeroarity/0
  fn zeroarity() i64 {
    return 47;
  }

  /// this function
  /// has two lines of document.
  /// nif: twoliner/0
  fn twoliner() i64 {
    return 42;
  }
  """

  @doc """
  positive_control
  """
  def positive_control, do: "positive_control"
end
