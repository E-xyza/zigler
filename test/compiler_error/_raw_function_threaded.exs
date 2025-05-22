defmodule ZiglerTest.CompilerError.RawFunctionThreaded do
  use Zig, otp_app: :zigler, nifs: [raw: [:threaded, arity: 1]]

  ~Z"""
  const beam = @import("beam");
  pub fn raw(_: beam.env, _: c_int, args: [*]beam.term) beam.term {
      return args[0];
  }
  """
end
