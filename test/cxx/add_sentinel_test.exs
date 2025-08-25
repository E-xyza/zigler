# if System.get_env("DISABLE_TESTS", "false") == "true" do
#
# defmodule ZiglerTest.CXX.AddSentinelTest do
#   use ZiglerTest.IntegrationCase, async: true
#
#   use Zig,
#     otp_app: :zigler,
#     easy_c: "add_sentinel.h",
#     c: [include_dirs: "include", src: "src2/add_sentinel.c"],
#     nifs: [count_string: [params: %{0 => :sentinel}]]
#
#   test "can specify parameter type" do
#     assert 5 = count_string("hello")
#   end
# end
#
# end
