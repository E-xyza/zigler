defmodule ZiglerTest.Snapshot.LongRunningTest do

  use ExUnit.Case, async: true

  describe "resource_struct/1 generates a zig struct" do
    test "for a 0-arity function" do
      assert """
      const __foo_cache__ = struct {
        env: beam.env,
        self: beam.pid,
        ref: beam.term,
        thread: *std.Thread,
        res: beam.term
      };
      """ == Zigler.Code.LongRunning.cache_struct(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 1-arity function" do
      assert """
      const __foo_cache__ = struct {
        env: beam.env,
        self: beam.pid,
        ref: beam.term,
        thread: *std.Thread,
        v0: i64,
        res: beam.term
      };
      """ == Zigler.Code.LongRunning.cache_struct(%{name: :foo, params: ["i64"], retval: "i64"})
    end

    test "for a 2-arity function" do
      assert """
      const __foo_cache__ = struct {
        env: beam.env,
        self: beam.pid,
        ref: beam.term,
        thread: *std.Thread,
        v0: i64,
        v1: f64,
        res: beam.term
      };
      """ == Zigler.Code.LongRunning.cache_struct(%{name: :foo, params: ["i64", "f64"], retval: "i64"})
    end
  end

  describe "packer_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_pack__(cache_ret: **__foo_cache__, env: beam.env) !void {

        var cache = try beam.allocator.create(__foo_cache__);
        errdefer { beam.allocator.destroy(cache); }

        cache.env = env;
        cache.self = try beam.self(env);
        cache.ref = try beam.make_ref(env);
        cache.res = try beam.resource.create(i64, env, foo_resource, undefined);


        cache.thread = try std.Thread.spawn(cache, __foo_harness__);
        cache_ret.* = cache;
      }
      """ == Zigler.Code.LongRunning.packer_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 2-arity function" do
      assert """
      fn __foo_pack__(cache_ret: **__foo_cache__, env: beam.env, v0: i64, v1: f64) !void {

        var cache = try beam.allocator.create(__foo_cache__);
        errdefer { beam.allocator.destroy(cache); }

        cache.env = env;
        cache.self = try beam.self(env);
        cache.ref = try beam.make_ref(env);
        cache.res = try beam.resource.create(i64, env, foo_resource, undefined);

        cache.v0 = v0;
        cache.v1 = v1;

        cache.thread = try std.Thread.spawn(cache, __foo_harness__);
        cache_ret.* = cache;
      }
      """  == Zigler.Code.LongRunning.packer_fn(%{name: :foo, params: ["i64", "f64"], retval: "i64"})
    end
  end

  describe "launch_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_launch__(env: beam.env) beam.term {
        var cache: *__foo_cache__ = undefined;

        __foo_pack__(&cache, env) catch {
          return beam.raise(env, beam.make_atom(env, "error"[0..]));
        };

        return e.enif_make_tuple(env, 2, cache.ref, cache.res);
      }
      """ == Zigler.Code.LongRunning.launcher_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 2-arity function" do
      assert """
      fn __foo_launch__(env: beam.env, v0: i64, v1: f64) beam.term {
        var cache: *__foo_cache__ = undefined;

        __foo_pack__(&cache, env, v0, v1) catch {
          return beam.raise(env, beam.make_atom(env, "error"[0..]));
        };

        return e.enif_make_tuple(env, 2, cache.ref, cache.res);
      }
      """ == Zigler.Code.LongRunning.launcher_fn(%{name: :foo, params: ["i64", "f64"], retval: "i64"})
    end
  end

  describe "harness_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_harness__(cache: *__foo_cache__) void {
        defer beam.allocator.destroy(cache);

        var result = foo();

        beam.resource.update(i64, cache.env, foo_resource, cache.res, result)
          catch |err| return;

        var res = e.enif_send(null, &cache.self, cache.env, cache.ref);
      }
      """ == Zigler.Code.LongRunning.harness_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 2-arity function" do
      assert """
      fn __foo_harness__(cache: *__foo_cache__) void {
        defer beam.allocator.destroy(cache);

        var result = foo(cache.v0, cache.v1);

        beam.resource.update(i64, cache.env, foo_resource, cache.res, result)
          catch |err| return;

        var res = e.enif_send(null, &cache.self, cache.env, cache.ref);
      }
      """ == Zigler.Code.LongRunning.harness_fn(%{name: :foo, params: ["i64", "f64"], retval: "i64"})
    end
  end

  describe "fetch_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_fetch__(env: beam.env, resource: beam.term) beam.term {
        var result = beam.resource.fetch(i64, env, add_resource, resource)
          catch |err| return beam.raise(env, beam.make_atom(env, "resource error"[0..]));

        // release the resource.
        beam.resource.release(env, add_resource, resource);

        return beam.make_i64(env, result);
      }
      """ == Zigler.Code.LongRunning.fetch_fn(%{name: :foo, params: [], retval: "i64"})
    end
  end
end
