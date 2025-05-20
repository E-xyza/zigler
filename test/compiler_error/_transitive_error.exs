defmodule ZiglerTest.CompilerError.TransitiveError do
  use Zig, otp_app: :zigler

  ~Z"""
  pub const transitive_error = @import("_transitive_error.zig").transitive_error;
  """
end
