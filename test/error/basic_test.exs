defmodule ZiglerTest.ErrorReturn.BasicTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :erroring

  use Zig, otp_app: :zigler

  ~Z"""
  pub noinline fn erroring() !void {
      return error.my_error;
  }
  """

  @expected_file "test/error/basic_test.exs"

  @sysarch :system_architecture |> :erlang.system_info() |> to_string()
  case {:os.type(), String.contains?(@sysarch, "x86_64")} do
    {{:unix, :darwin}, true} ->
      # see: https://github.com/ziglang/zig/issues/25157
      @error_disabled true
    _ ->
      @error_disabled false
  end

  test "when you call an erroring function" do
    error =
      try do
        erroring()
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      else
        _ -> raise "error not raised"
      end

    if @error_disabled do
      assert %{payload: :my_error} = error
    else
      assert %{payload: :my_error, stacktrace: [head | _]} = error
      assert {__MODULE__, :erroring, [:...], [file: @expected_file, line: 10]} = head
    end
  end

  ~Z"""
  pub fn transitive_error() !void {
      return erroring();
  }
  """

  test "when you call a transitively erroring function" do
    error =
      try do
        transitive_error()
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      else
        _ -> raise "error not raised"
      end

    if @error_disabled do
      assert %{payload: :my_error} = error
    else
      assert %{
               payload: :my_error,
               stacktrace: [
                 {__MODULE__, :erroring, [:...], [file: @expected_file, line: 10]},
                 {__MODULE__, :transitive_error, [:...], [file: @expected_file, line: 46]} | _
               ]
             } = error
    end
  end

  ~Z"""
  pub fn transitive_file_error() !void {
      return @import("transitive_error.zig").erroring();
  }
  """

  test "when you call a transitively erroring function that's in another file" do
    error =
      try do
        transitive_file_error()
      rescue
        e in ErlangError ->
          %{payload: e.original, stacktrace: __STACKTRACE__}
      else
        _ -> raise "error not raised"
      end

    transitive_error_file = Path.expand("transitive_error.zig", __DIR__)

    if @error_disabled do
      assert %{payload: :my_error} = error
    else
      assert %{
               payload: :my_error,
               stacktrace: [
                 {__MODULE__, :erroring, [:...], [file: ^transitive_error_file, line: 2]},
                 {__MODULE__, :transitive_file_error, [:...], [file: @expected_file, line: 76]}
                 | _
               ]
             } = error
    end
  end
end
