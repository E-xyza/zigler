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
    error =
      try do
        threaded(42)
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      end

    assert %{payload: :BadNumber, stacktrace: [head, next | _]} = error

    expected_file = Path.absname(__ENV__.file)

    assert {__MODULE__, :threaded, [:...], [file: ^expected_file, line: 13]} = head
    assert {__MODULE__, :threaded, 1, [file: ^expected_file, line: 12]} = next
  end
end
