defmodule Zigler.Nif.Threaded do
  @moduledoc """
  Generates code for threaded nifs.

  threaded functions require several parts to get right.

  0. a zig struct needs to be that holds arguments relvant to the
  threaded nif, these arguments need to be cleaned up in a sane
  fashion once the nif has completed running. This struct is going to be
  argetric on the nif arguments, and will be packed into a BEAM
  resource.

  1. a `packer` function which takes the beam arguments and shoves
  them into a zig struct to be stored in a BEAM resource, then launches
  the function, returning the resource.

  2. a `launcher` function which runs the `packer` wrapping the errors
  from the launch function.  The launch function must be a nif function,
  as it will be called from the BEAM.

  3. a `harness` function which is passed the resource struct, and is
  responsible for updating the resource, sending back a "finished"
  message to parent context.  The harness function will be responsible
  for wrapping the declared nif function.

  4. a `cleanup` function which is responsible for cleaning up the
  resource object once the thread has completed its task.
  """

  alias Zigler.Parser.Nif
  alias Zigler.Typespec
  alias Zigler.Nif.Adapter

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
      {:ok, ref} = unquote(launcher_call)

      try do
        receive do
          {^ref, return} -> return
          {:error, :enomem} ->
            raise "no memory"
          {:error, :function_clause} ->
            raise %FunctionClauseError{
              module: __MODULE__,
              function: unquote(name),
              arity: unquote(arity)
            }
        end
      rescue
        error -> reraise error, __STACKTRACE__
      after
        unquote(cleanup name)(ref)
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
  def thread_name(fn_name), do: String.to_atom("__#{fn_name}_name__")
  def packer(fn_name), do: String.to_atom("__#{fn_name}_pack__")
  def launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  def harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")
  def cleanup(fn_name), do: String.to_atom("__#{fn_name}_cleanup__")

  @env_types ["beam.env", "?*e.ErlNifEnv"]

  def cache_struct(nif) do
    extra_lines = nif.args
    |> Enum.reject(&(&1 in @env_types))
    |> Enum.with_index
    |> Enum.map(fn {_type, idx} -> "  arg#{idx}: beam.term,\n" end)

    test_msg = """
        _ = beam.send(env, cache.parent, beam.make_atom(env, "thread_freed"));
    """

    """
    const #{cache nif.name} = struct {
      env: beam.env,
      parent: beam.pid,
      thread: e.ErlNifTid,
      name: [:0]u8,
      this: beam.term,
    #{extra_lines}};

    /// resource: #{cache_ptr nif.name} definition
    const #{cache_ptr nif.name} = ?*#{cache nif.name};

    /// resource: #{cache_ptr nif.name} cleanup
    fn #{cache_cleanup nif.name}(env: beam.env, cache_res_ptr: *#{cache_ptr nif.name}) void {
      if (cache_res_ptr.*) | cache | {
        // always destroy the allocated memory for the cache.
        defer beam.allocator.destroy(cache);

        // always destroy the beam environment for the thread
        defer e.enif_clear_env(cache.env);

        // perform thread join to clean up any internal references to this thread.
        var tjoin = e.enif_thread_join(cache.thread, null);
    #{if Mix.env == :test, do: test_msg}  }
    }
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
    transfer_args = nif.args
    |> Enum.reject(&(&1 in @env_types))
    |> Enum.with_index
    |> Enum.map(fn {_, index} -> "  cache.arg#{index} = e.enif_make_copy(new_env, argv[#{index}]);\n" end)

    """
    const #{thread_name nif.name} = "#{nif.name}-threaded";
    fn #{packer nif.name}(env: beam.env, argv: [*c] const beam.term) !beam.term {
      // create a resource that is ready to hold the pointer to the cache.
      var cache_ref = try __resource__.create(#{cache_ptr nif.name}, env, null);
      errdefer __resource__.release(#{cache_ptr nif.name}, env, cache_ref);

      // allocate space for the cache and obtain its pointer.
      var cache = try beam.allocator.create(#{cache nif.name});
      errdefer beam.allocator.destroy(cache);

      // allocate space for the name of this thread, then copy it, and null-terminate it.
      var thread_name: []u8 = try beam.allocator.alloc(u8, #{thread_name nif.name}.len + 1);
      std.mem.copy(u8, thread_name, #{thread_name nif.name});
      thread_name[#{thread_name nif.name}.len] = 0;

      // update the stored pointer.
      try __resource__.update(#{cache_ptr nif.name}, env, cache_ref, cache);

      // drop contents into the cache; all items in the cache will be placed into
      // the new environment created specially for the thread.
      var new_env = if (e.enif_alloc_env()) | env_ | env_ else return beam.ThreadError.LaunchError;

      cache.env = new_env;
      cache.parent = try beam.self(env);
      cache.this = e.enif_make_copy(new_env, cache_ref);
    #{transfer_args}
      if (0 == e.enif_thread_create(
          thread_name.ptr,
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

    get_clauses = Adapter.get_clauses(nif, &bail/1, &"cache.arg#{&1}")

    result_term = if nif.retval == "void" do
      "beam.make_ok(cache.env)"
    else
      Adapter.make_clause(nif.retval, "result", "cache.env")
    end

    """
    export fn #{harness nif.name}(cache_q: ?*c_void) ?*c_void {
      var cache: *#{cache nif.name} =
        @ptrCast(*#{cache nif.name},
          @alignCast(@alignOf(#{cache nif.name}), cache_q.?));

      // always release the reference to the desired resource
      defer __resource__.release(#{cache_ptr nif.name}, cache.env, cache.this);

      // take out a reference against the desired resource
      __resource__.keep(#{cache_ptr nif.name}, cache.env, cache.this) catch return null;
      var env = cache.env;

    #{get_clauses}  // execute the nif function
      #{result_assign}#{nif.name}(#{Adapter.args nif});

      var result_term = #{result_term};
      var sent_term = e.enif_make_tuple(cache.env, 2, cache.this, result_term);
      _ = beam.send_advanced(null, cache.parent, cache.env, sent_term);

      return null;
    }
    """
  end

  defp bail(:oom), do: """
  {
        _ = beam.send_advanced(
          null,
          cache.parent,
          cache.env,
          beam.make_error_term(env, beam.make_atom(env, "enomem"[0..])));
        return null;
      }
  """
  defp bail(:function_clause), do: """
  {
        _ = beam.send_advanced(
          null,
          cache.parent,
          cache.env,
          beam.make_error_term(env, beam.make_atom(env, "function_clause"[0..])));
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
  def zig_adapter(nif) do
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
