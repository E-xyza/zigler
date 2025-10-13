defmodule ZiglerTest.Concurrency.ThreadedAutomaticErroringTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :threaded
  @moduletag :erroring

  use Zig, otp_app: :zigler, nifs: [threaded: [:threaded]]

  ~Z"""
  const beam = @import("beam");

  const ThreadError = error{BadNumber};

  pub fn threaded(number: i32) !i32 {
      if (number == 42) {
          return error.BadNumber;
      }
      return number + 1;
  }
  """

  test "threaded function can succeed" do
    assert 48 = threaded(47)
  end

  test "threaded function can error" do
    error =
      try do
        threaded(42)
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      end

    assert %{payload: :BadNumber, stacktrace: [head | _] = stacktrace} = error

    if Zig._errors_available?() do
      expected_file = Path.relative_to_cwd(__ENV__.file)
      assert {__MODULE__, :threaded, [:...], [file: ^expected_file, line: 16]} = head
    else
      expected_file = Path.relative_to_cwd(__ENV__.file) |> to_charlist
      assert {__MODULE__, :threaded, 1, [file: ^expected_file, line: 14]} = head
    end

    refute Enum.any?(stacktrace, fn {_, function, _, _} -> function == :"threaded-join" end)
  end
end
