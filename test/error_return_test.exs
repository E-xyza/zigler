defmodule ZiglerTest.ErrorReturnTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erroring

  use Zig, otp_app: :zigler

  @expected_file "test/error_return_test.exs"

  defmacrop assert_stacktrace(stacktrace_e, stacktrace_ne, code) do
    components = [
      payload: :my_error,
      stacktrace: List.wrap(if Zig._errors_available?(), do: stacktrace_e, else: stacktrace_ne)
    ]

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
    file = Macro.expand(file, __CALLER__)

    if Zig._errors_available?() do
      quote do
        {__MODULE__, unquote(function), [:...], [file: unquote(file), line: unquote(line)]}
      end
    else
      quote do
        {__MODULE__, unquote(function), 0,
         [file: unquote(to_charlist(file)), line: unquote(line)]}
      end
    end
  end

  ~Z"""
  pub noinline fn erroring() !void {
      return error.my_error;
  }
  """

  test "when you call an erroring function" do
    assert_stacktrace([info(:erroring, 46) | _], [info(:erroring, 45) | _], erroring())
  end

  ~Z"""
  pub fn transitive_error() !void {
      return erroring();
  }
  """

  test "when you call a transitively erroring function" do
    assert_stacktrace(
      [info(:erroring, 46), info(:transitive_error, 56) | _],
      [info(:transitive_error, 55) | _],
      transitive_error()
    )
  end

  ~Z"""
  pub fn transitive_file_error() !void {
      return @import("transitive_error.zig").erroring();
  }
  """

  @transitive_error_file Path.expand("transitive_error.zig", __DIR__)
  test "when you call a transitively erroring function that's in another file" do
    assert_stacktrace(
      [
        info(:erroring, @transitive_error_file, 2),
        info(:transitive_file_error, 70) | _
      ],
      [info(:transitive_file_error, 69) | _],
      transitive_file_error()
    )
  end
end
