defmodule ZiglerTest.CompilerError.SyntaxError do
  use Zig, otp_app: :zigler

  # these comments add space to make sure the line numbers are correct.

  ~Z"""
  pub fn missing_a_semicolon() u32 {
    return 42 // note a semicolon is missing here.
  }
  """
end
