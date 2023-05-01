defmodule ZiglerTest.ErrorReturn.BasicTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");
  """
end
