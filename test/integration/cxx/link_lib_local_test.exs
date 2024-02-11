ZiglerTest.SupportContent.ensure_lib("test/integration/cxx/libtest.a", "test/integration/cxx/test.zig")

defmodule ZiglerTest.Integration.CXX.LinkLibLocalTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    link_lib: "libtest.a"

  ~Z"""
  extern fn add_one(c_int) c_int;

  pub fn test_add_one(number: c_int) c_int {
      return add_one(number);
  }
  """

  test "can link a local library" do
    assert 48 == test_add_one(47)
  end
end