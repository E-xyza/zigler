File.rm_rf!(Zig.Builder.staging_directory(ZiglerTest.PrecompiledTest))

defmodule ZiglerTest.Compiled do
  use Zig, otp_app: :zigler

  ~Z"""
  pub fn add_one(x: u32) u32 {
      return x + 1;
  }
  """
end

defmodule ZiglerTest.PrecompiledTest do
  use ExUnit.Case, async: true

  require ZiglerTest.Compiled

  use Zig, otp_app: :zigler, precompiled: "./priv/lib/Elixir.ZiglerTest.Compiled.so"

  ~Z"""
  pub fn add_one(x: u32) u32 {
    return x + 1;
  }
  """

  test "staging directory doesn't exist" do
    refute File.dir?(Zig.Builder.staging_directory(ZiglerTest.PrecompiledTest))
  end

  test "function works" do
    assert add_one(47) == 48
  end
end
