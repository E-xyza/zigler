defmodule ZiglerTest.Types.Errors.BeamEnvFails do
  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  pub fn forbidden(env: beam.env) void {
    _ = env;
  }
  """
end
