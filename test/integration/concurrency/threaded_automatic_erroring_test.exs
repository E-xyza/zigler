defmodule ZiglerTest.Concurrency.ThreadedAutomaticErroringTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, nifs: [threaded: [:threaded]]

  ~Z"""
  const std = @import("std");
  const beam = @import("beam");

  const ThreadError = error{BadNumber};

  pub fn threaded(number: i32) !i32 {
    if (number == 42) { return error.BadNumber; }
    return number + 1;
  }
  """

  test "threaded function can error" do
    assert 48 = threaded(47)

    assert_raise ErlangError, "foo", fn ->
      threaded(42)
    end
  end
end
