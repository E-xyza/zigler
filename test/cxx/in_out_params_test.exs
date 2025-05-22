defmodule ZiglerTest.CXX.InOutParamsTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    nifs: [
      ...,
      in_out_int: [params: %{0 => :in_out}, return: [error: :make_error]]
    ]

  ~Z"""
  const c = @cImport(@cInclude("c.h"));

  pub fn in_out_int(value: *u32) c_int {
      if (value.* == 42) return 1;
      value.* = value.* + 1;
      return 0;
  }

  pub fn make_error(e: c_int) !void {
      if (e == 1) return error.badnumber;
  }
  """

  test "works in the normal case" do
    assert 48 = in_out_int(47)
  end

  test "fails in the error case" do
    assert_raise ErlangError, "Erlang error: :badnumber", fn ->
      in_out_int(42)
    end
  end

  test "make_error is ignored" do
    refute function_exported?(__MODULE__, :make_error, 1)
  end
end
