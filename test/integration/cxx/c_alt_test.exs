defmodule ZiglerTest.Integration.CXX.CAltTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, include_dir: "include", c_src: {"src/c.c", ["-Wall"]}

  ~Z"""
  const c = @cImport(@cInclude("c.h"));

  pub fn plus_one(x: c_int) c_int {
    return c.plus_one(x);
  }
  """

  test "c plus one" do
    assert 48 = plus_one(47)
  end
end
