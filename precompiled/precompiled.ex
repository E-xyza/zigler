defmodule ZiglerTest.MultiplatformPrecompiledTest do
  @moduledoc """
  This module provides a basic module that can be precompiled using `mix zig.precompile` command.
  The precompiled content should be uploaded as Zigler repository "releases" and can be used for
  `test/precompiled_test.exs` in `ZiglerTest.MultiplatformPrecompiledTest`.
  """

  use Zig, otp_app: :zigler

  ~Z"""
  pub fn add_one(x: u32) u32 {
    return x + 1;
  }
  """
end
