defmodule ZiglerTest.ErrorReturn.BasicTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  const MyError = error{my_error};

  fn nested_error() !void {
    return error.my_error;
  }

  pub fn basic_error_return() !void {
    // some extra space here
    return nested_error();
  }
  """

  test "when you get a basic error" do
    error =
      try do
        basic_error_return()
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      end

    assert %{payload: :my_error, stacktrace: [head, next | _]} = error

    expected_file = Path.absname(__ENV__.file)

    assert {__MODULE__, :basic_error_return, [:...], [file: ^expected_file, line: 18]} = next
    assert {__MODULE__, :nested_error, [:...], [file: ^expected_file, line: 13]} = head
  end
end
