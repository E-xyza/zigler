defmodule ZiglerTest.ErrorReturn.BasicTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  const MyError = error{my_error};

  pub fn basic_error_return() !void {
    return error.my_error;
  }
  """

  test "when you get a basic error" do
    error =
      try do
        basic_error_return()
      rescue
        e in ErlangError ->
          e |> IO.inspect(label: :e)
      end

    assert %{original: :my_error} = error
  end
end
