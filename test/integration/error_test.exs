defmodule ZiglerTest.Integration.ErrorTest do
  use ExUnit.Case, async: true
  use Zig, link_libc: true

  @basic_error_line __ENV__.line + 5
  ~Z"""
  /// nif: void_error/1
  fn void_error(input: i64) !void {
    if (input != 47) {
      return error.BadInput;
    }
  }
  """

  def foo do
    raise __MODULE__.ZigError, message: "foo", error_return_trace: []
  end

  test "for the void error case" do
    assert nil == void_error(47)

    {error, stacktrace} =
      try do
        void_error(42)
      rescue
        error in __MODULE__.ZigError ->
          Exception.blame(:error, error, __STACKTRACE__)
      end

    assert Exception.message(error) == "#{inspect __MODULE__}.void_error/1 returned the zig error `.BadInput`"

    assert [{
      :.., :void_error, [:...],
    [
      file: "test/integration/error_test.exs",
      line: @basic_error_line
    ]} | _] = stacktrace
  end

  @nested_error_line __ENV__.line + 4
  ~Z"""
  /// nif: nested_error/1
  fn nested_error(input: i64) !void {
    return void_error(input);
  }
  """

  test "for the nested error case" do
    assert nil == nested_error(47)

    {error, stacktrace} =
      try do
        nested_error(42)
      rescue
        error in __MODULE__.ZigError ->
          Exception.blame(:error, error, __STACKTRACE__)
      end

    assert Exception.message(error) == "#{inspect __MODULE__}.nested_error/1 returned the zig error `.BadInput`"

    assert [{
      :.., :void_error, [:...],
    [
      file: "test/integration/error_test.exs",
      line: @basic_error_line
    ]}, {
      :.., :nested_error, [:...],
    [
      file: "test/integration/error_test.exs",
      line: @nested_error_line
    ]} | _] = stacktrace
  end

  @union_error_line __ENV__.line + 5
  ~Z"""
  /// nif: union_error/1
  fn union_error(input: i64) !i64 {
    if (input == 42) {
      return error.BadInput;
    }
    return input;
  }
  """

  test "for the error set union case" do
    assert 47 == union_error(47)

    {error, stacktrace} =
      try do
        union_error(42)
      rescue
        error in __MODULE__.ZigError ->
          Exception.blame(:error, error, __STACKTRACE__)
      end

    assert Exception.message(error) == "#{inspect __MODULE__}.union_error/1 returned the zig error `.BadInput`"

    assert [{
      :.., :union_error, [:...],
    [
      file: "test/integration/error_test.exs",
      line: @union_error_line
    ]} | _] = stacktrace
  end

  @external_line __ENV__.line + 6
  ~Z"""
  const external = @import("error_external.zig");

  /// nif: external_error/0 threaded
  fn external_error() !void {
    return external.void_error();
  }
  """

  @tag :skip # we won't do file resolution yet.
  test "for external files" do
    {error, stacktrace} =
      try do
        external_error()
      rescue
        error in __MODULE__.ZigError ->
          Exception.blame(:error, error, __STACKTRACE__)
      end

    assert Exception.message(error) == "#{inspect __MODULE__}.external_error/0 returned the zig error `.ExternalError`"

    assert [{
      :.., :"error_external.void_error", [:...],
    [
      file: "test/integration/error_external.zig",
      line: 4
    ]}, {
      :.., :nested_error, [:...],
    [
      file: "test/integration/error_test.exs",
      line: @external_line
    ]} | _] = stacktrace
  end

  @threaded_line __ENV__.line + 5
  ~Z"""
  /// nif: threaded_error/1 threaded
  fn threaded_error(input: i64) !i64 {
    if (input == 42) {
      return error.BadInput;
    }
    return input;
  }
  """

  test "for threaded" do
    assert 47 == threaded_error(47)

    {error, stacktrace} =
      try do
        threaded_error(42)
      rescue
        error in __MODULE__.ZigError ->
          Exception.blame(:error, error, __STACKTRACE__)
      end

    assert Exception.message(error) == "#{inspect __MODULE__}.threaded_error/1 returned the zig error `.BadInput`"

    assert [{
      :.., :threaded_error, [:...],
    [
      file: "test/integration/error_test.exs",
      line: @threaded_line
    ]} | _] = stacktrace
  end

  @yielding_line __ENV__.line + 5
  ~Z"""
  /// nif: yielding_error/1 yielding
  fn yielding_error(input: i64) !i64 {
    if (input == 42) {
      return error.BadInput;
    }
    return input;
  }
  """

  test "for yielding" do
    assert 47 == yielding_error(47)

    {error, stacktrace} =
      try do
        yielding_error(42)
      rescue
        error in __MODULE__.ZigError ->
          Exception.blame(:error, error, __STACKTRACE__)
      end

    assert Exception.message(error) == "#{inspect __MODULE__}.yielding_error/1 returned the zig error `.BadInput`"

    assert [{
      :.., :yielding_error, [:...],
    [
      file: "test/integration/error_test.exs",
      line: @yielding_line
    ]} | _] = stacktrace
  end
end
