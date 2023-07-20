defmodule ZiglerTest.Integration.C.CTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, include_dir: "include", c_src: "src/*"

  ~Z"""
  const c = @cImport(@cInclude("c.h"));

  pub const plus_one = c.plus_one;
  """

  test "c plus one" do
    assert 48 = plus_one(47)
  end
end
