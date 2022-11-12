defmodule ZiglerTest.Types.SliceTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn bool_test(b: bool) bool {
    return !b;
  }
  """
end
