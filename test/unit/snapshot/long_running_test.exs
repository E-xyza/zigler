defmodule ZiglerTest.Snapshot.LongRunningTest do

  use ExUnit.Case, async: true

  alias Zigler.Code.LongRunning

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

      /// resource: __foo_cache_ptr__ definition
      const __foo_cache_ptr__ = ?*__foo_cache__;

      /// resource: __foo_cache_ptr__ cleanup
      fn __foo_cache_cleanup__(env: beam.env, cache_res_ptr: *__foo_cache_ptr__) void {
        if (cache_res_ptr.*) | cache_ptr | {
          beam.allocator.destroy(cache_ptr);
        }
      }
      """ == LongRunning.cache_struct(%{name: :foo, params: [], retval: "i64"})
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

      /// resource: __foo_cache_ptr__ definition
      const __foo_cache_ptr__ = ?*__foo_cache__;

      /// resource: __foo_cache_ptr__ cleanup
      fn __foo_cache_cleanup__(env: beam.env, cache_res_ptr: *__foo_cache_ptr__) void {
        if (cache_res_ptr.*) | cache_ptr | {
          beam.allocator.destroy(cache_ptr);
        }
      }
      """ == LongRunning.cache_struct(%{name: :foo, params: ["i64"], retval: "i64"})
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

      /// resource: __bar_cache_ptr__ definition
      const __bar_cache_ptr__ = ?*__bar_cache__;

      /// resource: __bar_cache_ptr__ cleanup
      fn __bar_cache_cleanup__(env: beam.env, cache_res_ptr: *__bar_cache_ptr__) void {
        if (cache_res_ptr.*) | cache_ptr | {
          beam.allocator.destroy(cache_ptr);
        }
      }
      """ == LongRunning.cache_struct(%{name: :bar, params: ["i64", "f64"], retval: "f64"})
    end
  end

  describe "launch_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      extern fn __foo_launch__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        return __foo_pack__(env, argv)
          catch beam.raise(env, beam.make_atom(env, "error"));
      }
      """ == LongRunning.launcher_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for function with a different name" do
      assert """
      extern fn __bar_launch__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        return __bar_pack__(env, argv)
          catch beam.raise(env, beam.make_atom(env, "error"));
      }
      """ == LongRunning.launcher_fn(%{name: :bar, params: ["i64"], retval: "i64"})
    end
  end

  describe "packer_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_pack__(env: beam.env, argv: [*c] const beam.term) !beam.term {
        var cache_term = try __resource__.create(__foo_cache_ptr__, env, null);
        errdefer __resource__.release(__foo_cache_ptr__, env, cache_term);

        var cache = try beam.allocator.create(__foo_cache__);
        try __resource__.update(__foo_cache_ptr__, env, cache_term, cache);

        var done_atom = beam.make_atom(env, "done");

        cache.env = env;
        cache.self = try beam.self(env);
        cache.response = e.enif_make_tuple(env, 2, done_atom, cache_term);

        cache.thread = try std.Thread.spawn(cache, __foo_harness__);

        return cache_term;
      }
      """ == LongRunning.packer_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 2-arity function with a different name" do
      assert """
      fn __foo_pack__(env: beam.env, argv: [*c] const beam.term) !beam.term {
        var cache_term = try __resource__.create(__foo_cache_ptr__, env, null);
        errdefer __resource__.release(__foo_cache_ptr__, env, cache_term);

        var cache = try beam.allocator.create(__foo_cache__);
        try __resource__.update(__foo_cache_ptr__, env, cache_term, cache);

        var done_atom = beam.make_atom(env, "done");

        cache.env = env;
        cache.self = try beam.self(env);
        cache.response = e.enif_make_tuple(env, 2, done_atom, cache_term);

        cache.arg0 = try beam.get_i64(env, argv[0]);
        cache.arg1 = try beam.get_f64(env, argv[1]);

        cache.thread = try std.Thread.spawn(cache, __foo_harness__);

        return cache_term;
      }
      """  == LongRunning.packer_fn(%{name: :foo, params: ["i64", "f64"], retval: "i64"})
    end
  end

  describe "harness_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      fn __foo_harness__(cache: *__foo_cache__) void {
        cache.result = foo();
        var _sent = beam.send(null, cache.self, null, cache.response);
      }
      """ == LongRunning.harness_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a 2-arity function with a different name" do
      assert """
      fn __bar_harness__(cache: *__bar_cache__) void {
        cache.result = bar(cache.arg0, cache.arg1);
        var _sent = beam.send(null, cache.self, null, cache.response);
      }
      """ == LongRunning.harness_fn(%{name: :bar, params: ["i64", "f64"], retval: "i64"})
    end

    test "for a void retval function" do
      assert """
      fn __foo_harness__(cache: *__foo_cache__) void {
        foo();
        var _sent = beam.send(null, cache.self, null, cache.response);
      }
      """ == LongRunning.harness_fn(%{name: :foo, params: [], retval: "void"})
    end
  end

  describe "fetcher_fn/1 generates a zig function" do
    test "for a function with a generic retval" do
      assert """
      extern fn __foo_fetch__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        var cache_q: ?*__foo_cache__ = __resource__.fetch(__foo_cache_ptr__, env, argv[0])
          catch return beam.raise_function_clause_error(env);
        defer __resource__.release(__foo_cache_ptr__, env, argv[0]);

        if (cache_q) | cache | {
          return beam.make_i64(env, cache.result);
        } else {
          return beam.raise_function_clause_error(env);
        }
      }
      """ == LongRunning.fetcher_fn(%{name: :foo, params: [], retval: "i64"})
    end

    test "for a function with void retval" do
      assert """
      extern fn __foo_fetch__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        __resource__.release(__foo_cache_ptr__, env, argv[0]);
        return beam.make_atom(env, "nil");
      }
      """ == LongRunning.fetcher_fn(%{name: :foo, params: [], retval: "void"})
    end
  end
end
