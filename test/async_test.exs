defmodule ZiglerTest.AsyncTest do
  use ExUnit.Case

  use Zigler, app: :zigler

  ~Z"""
  const std = @import("std");

  const AsyncAdd = struct {
    left: i64,
    right: i64,
    result: *i64,
  };

  fn async_add_task(left: i64, right: i64) i64 {
    return left + right;
  }

  const acp = * const AsyncAdd;

  fn async_add_wrapper(cache: acp) void {
    cache.result.* = async_add_task(cache.left, cache.right);
  }

  /// once again, do something terrible here.  We'll replace it with
  /// something else in another stage.
  var global_result: *i64 = undefined;

  /// nif: async_add/2
  fn async_add(left: i64, right: i64) void {
    global_result = beam.allocator.create(i64) catch |_| return;

    const cache = AsyncAdd{
      .left = left,
      .right = right,
      .result = global_result
    };

    const thread = std.Thread.spawn(&cache, async_add_wrapper) catch |_| return;
    //thread.wait();
    //return cache.result.*;
  }

  /// nif: async_fetch/0
  fn async_fetch() i64 {
    return global_result.*;
  }
  """

  # STAGE 3: can we do a naive async operation with two hits (first hit is trigger)
  # second hit is retrieve.

  @tag :one
  test "we can trigger the function" do
    async_add(40, 7)
    Process.sleep(100)
    assert 47 = async_fetch()
  end

end
