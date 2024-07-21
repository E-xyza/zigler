defmodule ZiglerTest.CompilerError.FunctionNotFound do
  use Zig, otp_app: :zigler, nifs: [not_found_function: []]

  ~Z"""
  pub fn should_fail_param(foo: u32) u32 {
      return foo;
  }
  """
end
