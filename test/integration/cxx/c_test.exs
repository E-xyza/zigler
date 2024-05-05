defmodule ZiglerTest.Integration.CXX.CTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :skip
  test "restore"

  # use Zig, otp_app: :zigler, c: [include_dir: "include", src: "src/*"]
  #
  # ~Z"""
  # const c = @cImport(@cInclude("c.h"));
  #
  # pub const plus_one = c.plus_one;
  # """
  #
  # test "c plus one" do
  #  assert 48 = plus_one(47)
  # end
end
