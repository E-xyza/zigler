defmodule ZiglerTest.AsyncTest do
  use ExUnit.Case

  use Zigler, app: :zigler

  ~Z"""
  const std = @import("std");

  // global shared memory, literally the worst, but we'll fix this.
  var global_result: i64 = 0;

  fn task_function(context: void) void {
    global_result = 47;
  }

  /// nif: async_tester/0
  fn async_tester() i64 {
    const thread = std.Thread.spawn({}, task_function) catch |_| return -1;
    thread.wait();
    return global_result;
  }
  """

  # STAGE 1: can we do a naive async operation?

  @tag :one
  test "we can trigger the function" do
    assert 47 == async_tester()
  end


end
