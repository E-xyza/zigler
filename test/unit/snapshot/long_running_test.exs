defmodule ZiglerTest.Snapshot.LongRunningTest do

  use ExUnit.Case, async: true

  describe "resource_struct/1 generates a zig struct" do
    test "for a 0-arity function" do
      assert """
      const __foo_cache__ = struct {
        env: beam.env,
        self: beam.pid,
        thread: *std.Thread,
        response: beam.term,
        result: i64
      };

      fn __foo_cache_cleanup__(env: beam.env, cache: ?*c_void) void {};
      """ == Zigler.Code.LongRunning.cache_struct(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 1-arity function" do
      assert """
      const __foo_cache__ = struct {
        env: beam.env,
        self: beam.pid,
        thread: *std.Thread,
        response: beam.term,
        arg0: i64,
        result: i64
      };

      fn __foo_cache_cleanup__(env: beam.env, cache: ?*c_void) void {};
      """ == Zigler.Code.LongRunning.cache_struct(%{name: :foo, params: ["i64"], retval: "i64"})
    end

    test "for a 2-arity function, with a different name and retval" do
      assert """
      const __bar_cache__ = struct {
        env: beam.env,
        self: beam.pid,
        thread: *std.Thread,
        response: beam.term,
        arg0: i64,
        arg1: f64,
        result: f64
      };

      fn __bar_cache_cleanup__(env: beam.env, cache: ?*c_void) void {};
      """ == Zigler.Code.LongRunning.cache_struct(%{name: :bar, params: ["i64", "f64"], retval: "f64"})
    end
  end


  describe "launch_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      extern fn __foo_launch__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        return __foo_pack__(env, argv)
          catch beam.raise(env, beam.make_atom(env, "error"));
      }
      """ == Zigler.Code.LongRunning.launcher_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for function with a different name" do
      assert """
      extern fn __bar_launch__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        return __bar_pack__(env, argv)
          catch beam.raise(env, beam.make_atom(env, "error"));
      }
      """ == Zigler.Code.LongRunning.launcher_fn(%{name: :bar, params: ["i64"], retval: "i64"})
    end
  end

  describe "packer_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_pack__(env: beam.env, argv: [*c] const beam.term) !beam.term {
        var resource = try beam.resource.create(__foo_cache__, env, __foo_cache_cleanup__, undefined);
        errdefer beam.resource.release(env, __foo_cache_cleanup__, resource);

        var cache = try beam.resource.fetch(__foo_cache__, env, __foo_cache_cleanup__);
        var done_atom = try beam.make_atom(env, "done");

        cache.env = env;
        cache.self = try beam.self(env);
        cache.response = e.enif_make_tuple(env, 2, done_atom, resource);

        cache.thread = try std.Thread.spawn(cache, __foo_harness__);
        return resource;
      }
      """ == Zigler.Code.LongRunning.packer_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 2-arity function with a different name" do
      assert """
      fn __foo_pack__(env: beam.env, argv: [*c] const beam.term) !beam.term {
        var resource = try beam.resource.create(__foo_cache__, env, __foo_cache_cleanup__, undefined);
        errdefer beam.resource.release(env, __foo_cache_cleanup__, resource);

        var cache = try beam.resource.fetch(__foo_cache__, env, __foo_cache_cleanup__);
        var done_atom = try beam.make_atom(env, "done");

        cache.env = env;
        cache.self = try beam.self(env);
        cache.response = e.enif_make_tuple(env, 2, done_atom, resource);

        cache.arg0 = try beam.get_i64(env, argv[0]);
        cache.arg1 = try beam.get_f64(env, argv[1]);

        cache.thread = try std.Thread.spawn(cache, __foo_harness__);
        return resource;
      }
      """  == Zigler.Code.LongRunning.packer_fn(%{name: :foo, params: ["i64", "f64"], retval: "i64"})
    end
  end

  describe "harness_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_harness__(cache: *__foo_cache__) void {
        cache.result = foo();
        beam.send(null, cache.self, cache.env, cache.response);
      }
      """ == Zigler.Code.LongRunning.harness_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 2-arity function with a different name" do
      assert """
      fn __bar_harness__(cache: *__bar_cache__) void {
        cache.result = bar(cache.arg0, cache.arg1);
        beam.send(null, cache.self, cache.env, cache.response);
      }
      """ == Zigler.Code.LongRunning.harness_fn(%{name: :bar, params: ["i64", "f64"], retval: "i64"})
    end
  end

  describe "fetcher_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      extern fn __foo_fetch__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var cache = beam.resource.fetch(__foo_cache__, env, __foo_cache_cleanup__, argv[0])
          catch return beam.raise_function_clause_error(env);
        defer beam.resource.release(env, __foo_cache_cleanup__, argv[0]);

        return beam.make_i64(env, cache.result) catch beam.raise_function_clause_error(env);
      }
      """ == Zigler.Code.LongRunning.fetcher_fn(%{name: :foo, params: [], retval: "i64"})
    end
  end
end
