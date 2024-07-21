defmodule ZiglerTest.Raw.ErrorRawMustHaveArity do
  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  pub fn raw(env: beam.env, count: c_int, args: [*]beam.term) beam.term {
      _ = count;
      _ = args;
      return beam.make(.ok, .{ .env = env });
  }
  """
end
