defmodule Zig.Nif.Threaded do
  @moduledoc """
  Generates code for threaded nifs.

  threaded functions require several parts to get right.

  0. a resource that holds space for arguments, result, and flags.  This
    is held on to by both the calling function

  1. a `packer` function which takes the beam arguments and shoves
    them into resource struct, then launches the function, returning the
    resource.

  2. a `launcher` function which runs the `packer` wrapping the errors
    from the launch function.  The launch function must be a nif function,
    as it will be called from the BEAM.

  3. a `harness` function which is passed the resource struct, and is
    responsible for unwrapping beam terms into function parameters.  This is
    what runs the nif function.

  4. a `catch` function which releases the resource reference, and signals to
    the parent process that it's finished.
  """

  alias Zig.Nif.Adapter
  alias Zig.Parser.Nif
  alias Zig.Typespec

  @behaviour Adapter

  #############################################################################
  ## Elixir Metaprogramming for threaded functions

  @impl true
  def beam_adapter(nif = %Nif{}) do
    typespec = Typespec.from_nif(nif)
    quote context: Elixir do
      unquote(typespec)
      unquote(threaded_main_fn(nif))
      unquote(threaded_launch_fn(nif))
      unquote(threaded_cleanup_fn(nif))
    end
  end

  defp threaded_main_fn(%{name: name, arity: arity}) do
    # note that the "define function" args should not take parentheses
    # but the "call" args must take parentheses.
    args = if arity == 0 do
      Elixir
    else
      for idx <- 1..arity, do: {String.to_atom("arg#{idx}"), [], Elixir}
    end
    launcher_call = if arity == 0 do
      {launcher(name), [], []}
    else
      {launcher(name), [], args}
    end

    internal_code = quote context: Elixir do
      case unquote(launcher_call) do
        {:ok, ref} ->
          receive do
            {:ok, {^ref, return}} ->
              unquote(cleanup name)(ref)
              return
            {:error, {^ref, :enomem}} ->
              unquote(cleanup name)(ref)
              raise "no memory"
            {:error, {^ref, :function_clause}} ->
              unquote(cleanup name)(ref)
              raise %FunctionClauseError{
                module: __MODULE__,
                function: unquote(name),
                arity: unquote(arity)
              }
            {:error, :thread_resource_error} ->
              raise "thread resource error for #{__ENV__.function}"
          end
        {:error, error} ->
          raise error
      end
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {name, [context: Elixir], args},
        [do: internal_code]
      ]}
  end

  defp threaded_launch_fn(%{name: name, arity: arity}) do
    text = "nif launcher for function #{name}/#{arity} not bound"

    args = if arity == 0 do
      Elixir
    else
      for _ <- 1..arity, do: {:_, [], Elixir}
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {launcher(name), [context: Elixir], args},
        [do: {:raise, [context: Elixir, import: Kernel], [text]}]
      ]}
  end

  defp threaded_cleanup_fn(%{name: name, arity: arity}) do
    text = "nif cleanup for function #{name}/#{arity} not bound"
    quote context: Elixir do
      def unquote(cleanup name)(_) do
        raise unquote(text)
      end
    end
  end

  #############################################################################
  ## Zig metaprogramming

  def cache_ptr(fn_name), do: String.to_atom("__#{fn_name}_cache_ptr__")
  def cache(fn_name), do: String.to_atom("__#{fn_name}_cache__")
  def cache_cleanup(fn_name), do: String.to_atom("__#{fn_name}_cache_cleanup__")
  def packer(fn_name), do: String.to_atom("__#{fn_name}_pack__")
  def name(fn_name), do: String.to_atom("__#{fn_name}_name__")
  def launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  def harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")
  def cleanup(fn_name), do: String.to_atom("__#{fn_name}_cleanup__")

  def cache_struct(nif) do
    test_msg = """
      _ = beam.send(env, cache.parent, beam.make_atom(env, "thread_freed"));
    """

    """
    const #{cache nif.name} = struct {
      env: beam.env = null,
      parent: beam.pid,
      thread: e.ErlNifTid,
      name: ?[:0] u8 = null,
      this: beam.term,
      args: ?[]beam.term = null
    };

    /// resource: #{cache_ptr nif.name} definition
    const #{cache_ptr nif.name} = *#{cache nif.name};

    /// resource: #{cache_ptr nif.name} cleanup
    fn #{cache_cleanup nif.name}(env: beam.env, cache_ptr: *#{cache_ptr nif.name}) void {
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
        defer e.enif_free_env(t_env);
      }

      // perform thread join to clean up any internal references to this thread.
      if (cache.thread) | thread | {
        _ = e.enif_thread_join(thread, null);
      }

    #{if Mix.env == :test, do: test_msg}}
    """
  end

  def launcher_fn(nif) do
    """
    export fn #{launcher nif.name}(env: beam.env, _argc: c_int, argv: [*c] const beam.term) beam.term {
      return #{packer nif.name}(env, argv) catch beam.make_error_binary(env, "launching nif");
    }
    """
  end

  def packer_fn(nif) do
    namelen = :erlang.size(Atom.to_string(nif.name)) + 9
    """
    const #{name nif.name} = "#{nif.name}-threaded";
    fn #{packer nif.name}(env: beam.env, argv: [*c] const beam.term) !beam.term {
      // allocate space for the cache and obtain its pointer.
      var cache = try beam.allocator.create(#{cache nif.name});
      errdefer beam.allocator.destroy(cache);

      // create a resource that is ready to hold the pointer to the cache.
      var cache_ref = try __resource__.create(#{cache_ptr nif.name}, env, cache);
      errdefer __resource__.release(#{cache_ptr nif.name}, env, cache_ref);

      // allocate space for the argument terms.
      cache.args = try beam.allocator.alloc(beam.term, #{nif.arity});

      // allocate space for the thread name, with a sentinel.
      cache.name = try beam.allocator.allocSentinel(u8, #{namelen}, 0);

      cache.env = if (e.enif_alloc_env()) | env_ | env_ else return beam.ThreadError.LaunchError;
      cache.parent = try beam.self(env);
      cache.this = e.enif_make_copy(cache.env, cache_ref);

      // copy the name and null-terminate it.
      std.mem.copy(u8, cache.name.?, #{name nif.name});

      // transfer the arguments over to the new environment.
      for (cache.args.?) |*arg, index| {
        cache.args.?[index] = e.enif_make_copy(cache.env, argv[index]);
      }

      if (0 == e.enif_thread_create(
          cache.name.?,
          &cache.thread,
          #{harness nif.name},
          @ptrCast(*c_void, cache),
          null)) {
        return beam.make_ok_term(env, cache_ref);
      } else return beam.ThreadError.LaunchError;
    }
    """
  end

  def harness_fn(nif) do
    result_assign = if nif.retval == "void", do: "", else: "var result = "

    get_clauses = Adapter.get_clauses(nif, &bail/1, &"cache.args.?[#{&1}]")

    result_term = Adapter.make_clause(nif.retval, "result", "cache.env")

    """
    export fn #{harness nif.name}(cache_q: ?*c_void) ?*c_void {
      var cache: *#{cache nif.name} =
        @ptrCast(*#{cache nif.name},
          @alignCast(@alignOf(#{cache nif.name}), cache_q.?));

      var env = cache.env;

      // check out the cache resource and lock its possession
      __resource__.keep(#{cache_ptr nif.name}, env, cache.this) catch {
        _ = beam.send_advanced(
          null,
          cache.parent,
          env,
          beam.make_error_atom(env, "thread_resource_error")
        );
        return null;
      };

      var result_term: beam.term = undefined;

      defer {
        // releasing the resource MUST come before sending the response, otherwise the
        // release event in this thread can collide with the release event in the main
        // thread and cause a segfault.

        __resource__.release(#{cache_ptr nif.name}, env, cache.this);

        _ = beam.send_advanced(
          null,
          cache.parent,
          env,
          result_term
        );
      }

      beam.yield_info = null;

    #{get_clauses}  // execute the nif function
      #{result_assign}nosuspend #{nif.name}(#{Adapter.args nif});
      result_term = beam.make_ok_term(
        env,
        e.enif_make_tuple(
          env,
          2,
          cache.this,
          #{result_term}
        )
      );

      return null;
    }
    """
  end

  defp bail(:oom), do: """
  {
        result_term =
          beam.make_error_term(env,
            e.enif_make_tuple(
              cache.env,
              2,
              cache.this,
              beam.make_atom(env, "enomem"[0..])
            )
          );
        return null;
      }
  """
  defp bail(:function_clause), do: """
  {
        result_term =
          beam.make_error_term(env,
            e.enif_make_tuple(
              cache.env,
              2,
              cache.this,
              beam.make_atom(env, "function_clause"[0..])
            )
          );
        return null;
      }
  """

  def cleanup_fn(nif) do
    """
    export fn #{cleanup nif.name}(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
      // release the resource and let it be garbage collected.
      defer __resource__.release(#{cache_ptr nif.name}, env, argv[0]);

      return beam.make_ok(env);
    }
    """
  end

  @impl true
  def zig_adapter(nif, _module) do
    [cache_struct(nif), "\n",
     launcher_fn(nif), "\n",
     packer_fn(nif), "\n",
     harness_fn(nif), "\n",
     cleanup_fn(nif)]
  end

  @impl true
  def nif_table_entries(nif) do
    """
      e.ErlNifFunc{
        .name = "#{launcher nif.name}",
        .arity = #{nif.arity},
        .fptr = #{launcher nif.name},
        .flags = 0,
      },
      e.ErlNifFunc{
        .name = "#{cleanup nif.name}",
        .arity = 1,
        .fptr = #{cleanup nif.name},
        .flags = 0,
      },
    """
  end

end
