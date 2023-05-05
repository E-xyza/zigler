defmodule ZiglerTest.ErrorReturn.BasicTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  const MyError = error{my_error};

  fn nested_error() !void {
    return error.my_error;
  }

  pub fn basic_error_return() !void {
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

    assert %{payload: :my_error, stacktrace: [head | _]} = error

    expected_file = Path.absname(__ENV__.file)

    assert {__MODULE__, :nested_error, :..., [file: ^expected_file, line: 13]} = head
  end
end
