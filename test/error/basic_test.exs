defmodule ZiglerTest.ErrorReturn.BasicTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erroring

  use Zig, otp_app: :zigler

  ~Z"""
  const MyError = error{my_error};

  pub fn errors() !void {
    return error.my_error;
  }
  """

  test "when you get a basic error" do
    error =
      try do
        errors()
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      end

    assert %{payload: :my_error, stacktrace: [head | _]} = error

    expected_file = Path.relative_to_cwd(__ENV__.file)

    assert {__MODULE__, :errors, [:...], [file: ^expected_file, line: 12]} = head
  end
end
