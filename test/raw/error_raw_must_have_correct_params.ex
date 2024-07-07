defmodule ZiglerTest.Raw.ErrorRawMustHaveCorrectParams do
  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  pub fn raw(env: beam.env, foo: u64, bar: usize) beam.term {
      _ = foo;
      _ = bar;
      return beam.make(.ok, .{.env = env});
  }
  """
end
