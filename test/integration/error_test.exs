defmodule ZiglerTest.Integration.ErrorTest do
  use ExUnit.Case, async: true
  use Zig, link_libc: true

  ~Z"""
  /// nif: void_error/1
  fn void_error(input: i64) !void {
    if (input != 47) {
      return error.BadInput;
    }
  }

  /// nif: raise_error/1
  fn raise_error(env: beam.env, input: i64) beam.term {
    void_error(input) catch | err | {
      var ert = @errorReturnTrace();
      var raise_content =
        beam.make_tuple(env, &[_]beam.term{
          beam.make_atom(env, @errorName(err)),
          beam.make_error_return_trace(env, ert)
        });

      return beam.raise(env, raise_content);
    };
    return beam.make_ok(env);
  }

  /// nif: union_error/1
  fn union_error(input: i64) !i64 {
    if (input == 42) {
      return error.BadInput;
    }
    return input;
  }
  """

  defmodule ZigError do
    defexception [:message, :error_return_trace]

    def blame(exception, stacktrace) do
      zig_errors =
        Enum.map(exception.error_return_trace, fn
          {module, fun, file, line} ->
            {module, fun, 0, [file: file, line: line]}
        end)

      {exception, zig_errors ++ stacktrace}
    end
  end

  defp void_error_shim(int) do
    raise_error(42)
  catch
    :error, {error, error_return_trace} ->
      raise ZigError,
        message: "zig code returned the error #{error}",
        error_return_trace: error_return_trace
  end

  test "for the void error case" do
    assert nil == void_error(47)

    {error, stacktrace} =
      try do
        void_error_shim(42)
      rescue
        error in ZigError ->
          Exception.blame(:error, error, __STACKTRACE__)
      end

    assert Exception.message(error)
    assert [{
      __MODULE__, :void_error, 0,
    [
      file: "/tmp/.zigler_compiler/test/Elixir.ZiglerTest.Integration.ErrorTest/Elixir.ZiglerTest.Integration.ErrorTest.zig",
      line: 9
    ]} | _] = stacktrace
  end

  @tag :skip
  test "for the error set union case" do
  end
end
