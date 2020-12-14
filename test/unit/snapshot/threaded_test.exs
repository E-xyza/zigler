defmodule ZiglerTest.Snapshot.ThreadedTest do

  use ExUnit.Case, async: true

  alias Zigler.Nif.Threaded

  @moduletag :snapshot

  describe "resource_struct/1 generates a zig struct" do
    test "for a 0-arity function" do
      assert """
      const __foo_cache__ = struct {
        env: beam.env = null,
        parent: beam.pid,
        thread: e.ErlNifTid,
        name: ?[:0] u8 = null,
        this: beam.term,
        args: ?[]beam.term = null
      };

      /// resource: __foo_cache_ptr__ definition
      const __foo_cache_ptr__ = *__foo_cache__;

      /// resource: __foo_cache_ptr__ cleanup
      fn __foo_cache_cleanup__(env: beam.env, cache_ptr: *__foo_cache_ptr__) void {
        var cache = cache_ptr.*;

        // always destroy the allocated arguments.
        if (cache.args) | args | {
          defer beam.allocator.free(args);
        }

        // always free the name.
        if (cache.name) | name | {
          defer beam.allocator.free(name);
        }

        // always destroy the allocated memory for the cache.
        defer beam.allocator.destroy(cache);

        // always destroy the beam environment for the thread
        if (cache.env) | t_env | {
          defer e.enif_clear_env(t_env);
        }

        // perform thread join to clean up any internal references to this thread.
        if (cache.thread) | thread | {
          _ = e.enif_thread_join(thread, null);
        }

        _ = beam.send(env, cache.parent, beam.make_atom(env, \"thread_freed\"));
      }
      """ == Threaded.cache_struct(%{name: :foo, args: [], retval: "i64"})
    end

    test "for a 2-arity function, with a different name and retval" do
      assert """
      const __bar_cache__ = struct {
        env: beam.env = null,
        parent: beam.pid,
        thread: e.ErlNifTid,
        name: ?[:0] u8 = null,
        this: beam.term,
        args: ?[]beam.term = null
      };

      /// resource: __bar_cache_ptr__ definition
      const __bar_cache_ptr__ = *__bar_cache__;

      /// resource: __bar_cache_ptr__ cleanup
      fn __bar_cache_cleanup__(env: beam.env, cache_ptr: *__bar_cache_ptr__) void {
        var cache = cache_ptr.*;

        // always destroy the allocated arguments.
        if (cache.args) | args | {
          defer beam.allocator.free(args);
        }

        // always free the name.
        if (cache.name) | name | {
          defer beam.allocator.free(name);
        }

        // always destroy the allocated memory for the cache.
        defer beam.allocator.destroy(cache);

        // always destroy the beam environment for the thread
        if (cache.env) | t_env | {
          defer e.enif_clear_env(t_env);
        }

        // perform thread join to clean up any internal references to this thread.
        if (cache.thread) | thread | {
          _ = e.enif_thread_join(thread, null);
        }

        _ = beam.send(env, cache.parent, beam.make_atom(env, "thread_freed"));
      }
      """ == Threaded.cache_struct(%{name: :bar, args: ["i64", "f64"], retval: "f64"})
    end
  end

  describe "launch_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      export fn __foo_launch__(env: beam.env, _argc: c_int, argv: [*c] const beam.term) beam.term {
        return __foo_pack__(env, argv) catch beam.make_error_binary(env, \"launching nif\");
      }
      """ == Threaded.launcher_fn(%{name: :foo, args: [], retval: "i64"})
    end

    test "for function with a different name" do
      assert """
      export fn __bar_launch__(env: beam.env, _argc: c_int, argv: [*c] const beam.term) beam.term {
        return __bar_pack__(env, argv) catch beam.make_error_binary(env, \"launching nif\");
      }
      """ == Threaded.launcher_fn(%{name: :bar, args: ["i64"], retval: "i64"})
    end
  end

  describe "packer_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      const __foo_name__ = "foo-threaded";
      fn __foo_pack__(env: beam.env, argv: [*c] const beam.term) !beam.term {
        // allocate space for the cache and obtain its pointer.
        var cache = try beam.allocator.create(__foo_cache__);
        errdefer beam.allocator.destroy(cache);

        // create a resource that is ready to hold the pointer to the cache.
        var cache_ref = try __resource__.create(__foo_cache_ptr__, env, cache);
        errdefer __resource__.release(__foo_cache_ptr__, env, cache_ref);

        // allocate space for the argument terms.
        cache.args = try beam.allocator.alloc(beam.term, 0);

        // allocate space for the thread name, with a sentinel.
        cache.name = try beam.allocator.allocSentinel(u8, 12, 0);

        cache.env = if (e.enif_alloc_env()) | env_ | env_ else return beam.ThreadError.LaunchError;
        cache.parent = try beam.self(env);
        cache.this = cache_ref;

        // copy the name and null-terminate it.
        std.mem.copy(u8, cache.name.?, __foo_name__);

        // transfer the arguments over to the new environment.
        for (cache.args.?) |*arg, index| {
          cache.args.?[index] = e.enif_make_copy(cache.env, argv[index]);
        }

        if (0 == e.enif_thread_create(
            cache.name.?,
            &cache.thread,
            __foo_harness__,
            @ptrCast(*c_void, cache),
            null)) {
          return beam.make_ok_term(env, cache_ref);
        } else return beam.ThreadError.LaunchError;
      }
      """ == Threaded.packer_fn(%{name: :foo, args: [], arity: 0, retval: "i64"})
    end

    test "for a 2-arity function with a different name" do
      assert """
      const __foo_name__ = "foo-threaded";
      fn __foo_pack__(env: beam.env, argv: [*c] const beam.term) !beam.term {
        // allocate space for the cache and obtain its pointer.
        var cache = try beam.allocator.create(__foo_cache__);
        errdefer beam.allocator.destroy(cache);

        // create a resource that is ready to hold the pointer to the cache.
        var cache_ref = try __resource__.create(__foo_cache_ptr__, env, cache);
        errdefer __resource__.release(__foo_cache_ptr__, env, cache_ref);

        // allocate space for the argument terms.
        cache.args = try beam.allocator.alloc(beam.term, 2);

        // allocate space for the thread name, with a sentinel.
        cache.name = try beam.allocator.allocSentinel(u8, 12, 0);

        cache.env = if (e.enif_alloc_env()) | env_ | env_ else return beam.ThreadError.LaunchError;
        cache.parent = try beam.self(env);
        cache.this = cache_ref;

        // copy the name and null-terminate it.
        std.mem.copy(u8, cache.name.?, __foo_name__);

        // transfer the arguments over to the new environment.
        for (cache.args.?) |*arg, index| {
          cache.args.?[index] = e.enif_make_copy(cache.env, argv[index]);
        }

        if (0 == e.enif_thread_create(
            cache.name.?,
            &cache.thread,
            __foo_harness__,
            @ptrCast(*c_void, cache),
            null)) {
          return beam.make_ok_term(env, cache_ref);
        } else return beam.ThreadError.LaunchError;
      }
      """  == Threaded.packer_fn(%{name: :foo, args: ["i64", "f64"], arity: 2, retval: "i64"})
    end
  end

  describe "harness_fn/1 generates a zig function" do
    test "for a 0-arity function" do
      assert """
      export fn __foo_harness__(cache_q: ?*c_void) ?*c_void {
        var cache: *__foo_cache__ =
          @ptrCast(*__foo_cache__,
            @alignCast(@alignOf(__foo_cache__), cache_q.?));

        // always clean up the environment.
        defer e.enif_free_env(cache.env);

        // check out the cache resource and lock its possession
        __resource__.keep(__foo_cache_ptr__, cache.env, cache.this) catch {
          // TODO: DO BETTER HERE.
          return null;
        };

        // always release the reference to the desired resource
        defer __resource__.release(__foo_cache_ptr__, cache.env, cache.this);

        var env = cache.env;

        // execute the nif function
        var result = foo();

        var result_term = beam.make_i64(cache.env, result);
        _ = beam.send_advanced(
          null,
          cache.parent,
          env,
          beam.make_ok_term(
            env,
            e.enif_make_tuple(
              env,
              2,
              cache.this,
              result_term
            )
          )
        );

        return null;
      }
      """ == Threaded.harness_fn(%{name: :foo, args: [], arity: 0, retval: "i64"})
    end

    test "for a 2-arity function with a different name" do
      assert """
      export fn __bar_harness__(cache_q: ?*c_void) ?*c_void {
        var cache: *__bar_cache__ =
          @ptrCast(*__bar_cache__,
            @alignCast(@alignOf(__bar_cache__), cache_q.?));

        // always clean up the environment.
        defer e.enif_free_env(cache.env);

        // check out the cache resource and lock its possession
        __resource__.keep(__bar_cache_ptr__, cache.env, cache.this) catch {
          XXXX
          return null;
        };

        // always release the reference to the desired resource
        defer __resource__.release(__bar_cache_ptr__, cache.env, cache.this);

        var env = cache.env;

        var __bar_arg0__ = beam.get_i64(env, cache.args.?[0])
          catch {
            _ = beam.send_advanced(
              null,
              cache.parent,
              cache.env,
              beam.make_error_term(env,
                e.enif_make_tuple(
                  cache.env,
                  2,
                  cache.this,
                  beam.make_atom(env, \"function_clause\"[0..])
                )
              )
            );
            return null;
          };
        var __bar_arg1__ = beam.get_f64(env, cache.args.?[1])
          catch {
            _ = beam.send_advanced(
              null,
              cache.parent,
              cache.env,
              beam.make_error_term(env,
                e.enif_make_tuple(
                  cache.env,
                  2,
                  cache.this,
                  beam.make_atom(env, \"function_clause\"[0..])
                )
              )
            );
            return null;
          };

        // execute the nif function
        var result = bar(__bar_arg0__, __bar_arg1__);

        var result_term = beam.make_i64(cache.env, result);
        _ = beam.send_advanced(
          null,
          cache.parent,
          env,
          beam.make_ok_term(
            env,
            e.enif_make_tuple(
              env,
              2,
              cache.this,
              result_term
            )
          )
        );

        return null;
      }
      """ == Threaded.harness_fn(%{name: :bar, args: ["i64", "f64"], arity: 2, retval: "i64"})
    end

    test "for a void retval function" do
      assert """
      export fn __foo_harness__(cache_q: ?*c_void) ?*c_void {
        var cache: *__foo_cache__ =
          @ptrCast(*__foo_cache__,
            @alignCast(@alignOf(__foo_cache__), cache_q.?));

        // always clean up the environment.
        defer e.enif_free_env(cache.env);

        // check out the cache resource and lock its possession
        __resource__.keep(__foo_cache_ptr__, cache.env, cache.this) catch {
          // TODO: DO BETTER HERE.
          return null;
        };

        // always release the reference to the desired resource
        defer __resource__.release(__foo_cache_ptr__, cache.env, cache.this);

        var env = cache.env;

        // execute the nif function
        foo();

        var result_term = beam.make_ok(cache.env);
        _ = beam.send_advanced(
          null,
          cache.parent,
          env,
          beam.make_ok_term(
            env,
            e.enif_make_tuple(
              env,
              2,
              cache.this,
              result_term
            )
          )
        );

        return null;
      }
      """ == Threaded.harness_fn(%{name: :foo, args: [], arity: 0, retval: "void"})
    end
  end

  describe "cleanup_fn/1 generates a zig function" do
    test "for any function" do
      assert """
      export fn __foo_cleanup__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
        // release the resource and let it be garbage collected.
        defer __resource__.release(__foo_cache_ptr__, env, argv[0]);

        return beam.make_ok(env);
      }
      """ == Threaded.cleanup_fn(%{name: :foo, args: [], arity: 0, retval: "void"})
    end
  end
end
