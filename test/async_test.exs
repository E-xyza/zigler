defmodule ZiglerTest.AsyncTest do
  use ExUnit.Case

  use Zigler, app: :zigler

  ~Z"""
  const std = @import("std");

  const AsyncAdd = struct {
    env: beam.env,
    self: beam.pid,
    ref: beam.term,
    left: i64,
    right: i64,
    result: *i64,
  };

  fn async_add_task(left: i64, right: i64) i64 {
    return left + right;
  }

  const acp = * AsyncAdd;

  /// wraps the async add trigger.  Gains ownership of the cache
  /// function.
  fn async_add_wrapper(cache: acp) void {
    cache.result.* = async_add_task(cache.left, cache.right);
    var res = e.enif_send(null, &cache.self, cache.env, cache.ref);
    // it's our responsibility to destroy the cache.
    beam.allocator.destroy(cache);
  }

  /// once again, do something terrible here.  We'll replace it with
  /// something else in another stage.
  var global_result: *i64 = undefined;

  /// nif: async_add/2
  fn async_add(env: beam.env, left: i64, right: i64) beam.term {

    global_result = beam.allocator.create(i64) catch |_| {
      return beam.raise(env, beam.make_atom(env, "error"[0..]));
    };

    //ownership of this cache will be passed to the wrapper function.
    var cache = beam.allocator.create(AsyncAdd) catch |_| {
      return beam.raise(env, beam.make_atom(env, "error"[0..]));
    };

    cache.env = env;

    cache.self = beam.self(env) catch |_| {
      beam.allocator.destroy(cache);
      return beam.raise(env, beam.make_atom(env, "error"[0..]));
    };
    cache.ref = beam.make_ref(env) catch |_| {
      beam.allocator.destroy(cache);
      return beam.raise(env, beam.make_atom(env, "error"[0..]));
    };

    cache.left = left;
    cache.right = right;
    cache.result = global_result;

    const thread = std.Thread.spawn(cache, async_add_wrapper) catch |_| {
      beam.allocator.destroy(cache);
      return beam.raise(env, beam.make_atom(env, "error"[0..]));
    };

    return cache.ref;
  }

  /// nif: async_fetch/0
  fn async_fetch() i64 {
    return global_result.*;
  }
  """

  # STAGE 5: reduce the surface area of interaction.

  @tag :one
  test "we can trigger the function" do
    ref = async_add(40, 7)
    receive do ^ref -> :ok end
    assert 47 = async_fetch()
  end

end
