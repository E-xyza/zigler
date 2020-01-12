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
  const ca = * const AsyncAdd;

  fn async_add_task(left: i64, right: i64) i64 {
    return left + right;
  }

  fn async_add_wrapper(cache: ca) void {
    cache.result.* = async_add_task(cache.left, cache.right);
  }

  /// nif: async_add/2
  fn async_add(left: i64, right: i64) i64 {
    var result = beam.allocator.create(i64) catch |_| return -1;
    defer beam.allocator.destroy(result);

    const cache = AsyncAdd{
      .left = left,
      .right = right,
      .result = result
    };

    const thread = std.Thread.spawn(&cache, async_add_wrapper) catch |_| return -1;
    thread.wait();
    return cache.result.*;
  }
  """

  # STAGE 2: can we do a naive async operation with (thread-safe) memory mutation to
  # return the value?

  @tag :one
  test "we can trigger the function" do
    assert 47 == async_add(40, 7)
  end


end
