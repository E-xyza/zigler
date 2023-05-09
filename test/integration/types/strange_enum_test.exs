defmodule ZiglerTest.Types.StrangeEnumTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    leak_check: true,
    otp_app: :zigler

  ~Z"""
  pub const ZeroEnums = enum{};

  pub fn zero_enum_input(_: ZeroEnums) void {}
  pub fn zero_enum_output(value: ZeroEnums) ZeroEnums { return value; }
  """
end
