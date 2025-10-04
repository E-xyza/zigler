defmodule ZiglerTest.ErrorReturnTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erroring

  use Zig, otp_app: :zigler

  @expected_file "test/error/basic_test.exs"

  defmacrop assert_stacktrace(stacktrace, code) do
    components =
      List.wrap(if Zig._errors_available?(), do: [stacktrace: stacktrace]) ++ [payload: :my_error]

    quote do
      assert %{unquote_splicing(components)} =
               (try do
                  unquote(code)
                rescue
                  e in ErlangError ->
                    %{payload: e.original, stacktrace: __STACKTRACE__}
                else
                  _ -> raise ExUnit.AssertionError, message: "error not raised"
                end)
    end
  end

  defmacrop info(function, file \\ @expected_file, line) do
    quote do
      {__MODULE__, unquote(function), [:...], [file: unquote(file), line: unquote(line)]}
    end
  end

  ~Z"""
  pub noinline fn erroring() !void {
      return error.my_error;
  }
  """

  test "when you call an erroring function" do
    assert_stacktrace([info(:erroring, 35) | _], erroring())
  end

  ~Z"""
  pub fn transitive_error() !void {
      return erroring();
  }
  """

  test "when you call a transitively erroring function" do
    assert_stacktrace([info(:erroring, 34), info(:transitive_error, 45) | _], transitive_error())
  end

  ~Z"""
  pub fn transitive_file_error() !void {
      return @import("transitive_error.zig").erroring();
  }
  """

  test "when you call a transitively erroring function that's in another file" do
    transitive_error_file = Path.expand("transitive_error.zig", __DIR__)

    assert_stacktrace([
      info(:erroring, ^transitive_error_file, 2),
      info(:transitive_file_error, 55) | _
    ], transitive_file_error())
  end
end
