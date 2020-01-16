defmodule ZiglerTest.AsyncTest do
  use ExUnit.Case

  use Zigler, app: :zigler, resources: [:async_add_resource]

  ~Z"""
  const std = @import("std");

  const AsyncAdd = struct {
    env: beam.env,
    self: beam.pid,
    ref: beam.term,
    res: beam.term,
    left: i64,
    right: i64,
  };

  fn async_add_task(left: i64, right: i64) i64 {
    return left + right;
  }

  const acp = * AsyncAdd;

  /// wraps the async add trigger.  Gains ownership of the cache
  /// function.
  fn async_add_wrapper(cache: acp) void {
    var result = async_add_task(cache.left, cache.right);

    // drop the result value into the resource.
    beam.resource.update(i64, cache.env, async_add_resource, cache.res, result)
      catch |err| return;

    var res = e.enif_send(null, &cache.self, cache.env, cache.ref);

    // it's our responsibility to destroy the cache.
    beam.allocator.destroy(cache);
  }

  /// nif: async_add/2
  fn async_add(env: beam.env, left: i64, right: i64) beam.term {

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
    cache.res = beam.resource.create(i64, env, async_add_resource, undefined) catch |_| {
      beam.allocator.destroy(cache);
      return beam.raise(env, beam.make_atom(env, "error"[0..]));
    };

    cache.left = left;
    cache.right = right;

    const thread = std.Thread.spawn(cache, async_add_wrapper) catch |_| {
      beam.allocator.destroy(cache);
      return beam.raise(env, beam.make_atom(env, "error"[0..]));
    };

    return e.enif_make_tuple(env, 2, cache.ref, cache.res);
  }

  /// nif: async_fetch/1
  fn async_fetch(env: beam.env, resource: beam.term) beam.term {
    var result = beam.resource.fetch(i64, env, async_add_resource, resource)
      catch |err| return beam.raise(env, beam.make_atom(env, "resource error"[0..]));

    // release the resource.
    beam.resource.release(env, async_add_resource, resource);

    return beam.make_i64(env, result);
  }

  /// destructor: async_add_resource
  extern fn destroy_async_add_resource(env: beam.env, obj: ?*c_void) void {
    // nothing needs to happen since this object is a single int64
  }

  /// nif: add/2 long
  fn add(left: i64, right: i64) i64 {
    return left + right;
  }
  """

  defp add_harness(left, right) do
    {ref, res} = async_add(left, right)
    receive do ^ref -> :ok end
    async_fetch(res)
  end

  # STAGE 6: use a resource to correctly fetch the result in a threadsafe
  # manner.

  #test "we can trigger the function" do
  #  assert 47 == add_harness(40, 7)
  #end

  @tag :long
  test "basic function with long_nif works" do
    assert 47 == add(40, 7)
  end

end
