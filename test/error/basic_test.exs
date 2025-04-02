defmodule ZiglerTest.ErrorReturn.BasicTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erroring

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

  @tag :skip
  test "when you get a basic error" do
    error =
      try do
        basic_error_return()
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      end

    assert %{payload: :my_error, stacktrace: [head, next | _]} = error

    expected_file = Path.relative_to_cwd(__ENV__.file)

    assert {__MODULE__, :basic_error_return, [:...], [file: ^expected_file, line: 20]} = next
    assert {__MODULE__, :nested_error, [:...], [file: ^expected_file, line: 15]} = head
  end
end
