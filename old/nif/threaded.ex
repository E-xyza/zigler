defmodule Zig.Nif.Threaded do
  @moduledoc """
  Generates code for threaded nifs.

  threaded functions require several parts to get right.

  0. a resource that holds space for arguments, result, and flags.  This
    is created by the calling function and owned by the process that called
    the function.

  1. a `packer` function which takes the beam arguments and shoves
    them into resource struct, then launches the function, returning the
    resource.

  2. a `launcher` function which runs the `packer` wrapping the errors
    from the launch function.  The launch function must be a nif function,
    as it will be called from the BEAM.

  3. a `harness` function which is passed the resource struct, and is
    responsible for unwrapping beam terms into function parameters.  This is
    what runs the nif function.

  ### Cleanup Strategy

  - If the thread is "naturally completed", then yield_info does not need to
  be cleaned up internally, it gets cleaned up by the harness function, and
  the `finished` value in yield_info is set to `true`.

  - If the thread has been "killed", then yield_info's `cancelled` value is set to
  true and the thread is given 500us (in 50 us chunks) to set its `finished`
  value to true.

  - If the thread has not self-terminated, it will be kept running, and its
  resources may not wind up being properly cleaned up.
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
    end
  end

  defp threaded_main_fn(%{name: name, arity: arity}) do
    # note that the "define function" args should not take parentheses
    # but the "call" args must take parentheses.
    args =
      if arity == 0 do
        Elixir
      else
        for idx <- 1..arity, do: {String.to_atom("arg#{idx}"), [], Elixir}
      end

    launcher_call =
      if arity == 0 do
        {launcher(name), [], []}
      else
        {launcher(name), [], args}
      end

    internal_code =
      quote context: Elixir do
        case unquote(launcher_call) do
          {:ok, ref} ->
            receive do
              {:ok, {^ref, return}} ->
                return

              {:error, {^ref, :enomem}} ->
                raise "no memory"

              {:error, {^ref, :function_clause}} ->
                raise %FunctionClauseError{
                  module: __MODULE__,
                  function: unquote(name),
                  arity: unquote(arity)
                }

              {:error, {^ref, map}} when is_exception(map) ->
                raise map

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

    args =
      if arity == 0 do
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

  #############################################################################
  ## Zig metaprogramming

  def cache_ptr(fn_name), do: String.to_atom("__#{fn_name}_cache_ptr__")
  def cache_cleanup(fn_name), do: String.to_atom("__#{fn_name}_cache_cleanup__")
  defp cache(fn_name), do: String.to_atom("__#{fn_name}_cache__")
  defp packer(fn_name), do: String.to_atom("__#{fn_name}_pack__")
  defp name(fn_name), do: String.to_atom("__#{fn_name}_name__")
  defp launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  defp harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")

  defp cache_struct(nif) do
    """
    const #{cache(nif.name)} = struct {
      thread: e.ErlNifTid,
      name: ?[:0] u8 = null,
      this: beam.term,
      args: ?[]beam.term = null,
      yield_info: beam.YieldInfo = undefined,
    };

    /// resource: #{cache_ptr(nif.name)} definition
    const #{cache_ptr(nif.name)} = *#{cache(nif.name)};

    /// resource: #{cache_ptr(nif.name)} cleanup
    fn #{cache_cleanup(nif.name)}(_: beam.env, cache_ptr: *#{cache_ptr(nif.name)}) void {
      // std.debug.assert(beam.yield_info == null);
      var cache = cache_ptr.*;

      // always destroy the allocated arguments.
      if (cache.args) | args | { beam.allocator.free(args); }

      // always free the name.
      if (cache.name) | name | { beam.allocator.free(name); }

      var attempts: usize = 50;
      while (@atomicRmw(
               beam.YieldState,
               &cache.yield_info.state,
               .Xchg,
               .Cancelled,
               .Monotonic) != .Finished) {
        attempts -= 1; // decrement first so there is no time between checking and giving up.
        if (attempts == 0) {
          @atomicStore(beam.YieldState, &cache.yield_info.state, .Abandoned, .Monotonic);
          return;
        }
        std.time.sleep(100_000);
      } else {
        _ = e.enif_thread_join(cache.thread, null);
      }

      // if it's finished, it's safe to store the allocated memory for the cache.
      beam.allocator.destroy(cache);
    }
    """
  end

  def launcher_fn(nif) do
    # note that this function needs to exist as a separate function to be a seam between the C ABI
    # that the erlang system demands.
    """
    export fn #{launcher(nif.name)}(env: beam.env, _: c_int, argv: [*c] const beam.term) beam.term {
      return #{packer(nif.name)}(env, argv) catch beam.make_error_binary(env, "launching nif");
    }
    """
  end

  def packer_fn(nif) do
    namelen = :erlang.size(Atom.to_string(nif.name)) + 9

    """
    const #{name(nif.name)} = "#{nif.name}-threaded";
    fn #{packer(nif.name)}(env: beam.env, argv: [*c] const beam.term) !beam.term {
      // std.debug.assert(beam.yield_info == null);
      beam.set_generic_self();
      // allocate space for the cache and obtain its pointer.
      var cache = try beam.allocator.create(#{cache(nif.name)});
      errdefer beam.allocator.destroy(cache);

      // create a resource that is ready to hold the pointer to the cache.
      var cache_ref = try __resource__.create(#{cache_ptr(nif.name)}, env, cache);
      defer {
        __resource__.release(#{cache_ptr(nif.name)}, env, cache_ref);
      }

      // allocate space for the argument terms.
      cache.args = try beam.allocator.alloc(beam.term, #{nif.arity});

      // allocate space for the thread name, with a sentinel.
      cache.name = try beam.allocator.allocSentinel(u8, #{namelen}, 0);

      // copy the name and null-terminate it.
      std.mem.copy(u8, cache.name.?, #{name(nif.name)});

      const new_environment = e.enif_alloc_env() orelse return beam.ThreadError.LaunchError;
      cache.yield_info = .{
        .environment = new_environment,
        .threaded = true,
        .parent = try beam.self(env),
      };

      cache.this = cache_ref;

      // transfer the arguments over to the new environment.
      for (cache.args.?) |_, index| {
        cache.args.?[index] = e.enif_make_copy(new_environment, argv[index]);
      }

      if (0 == e.enif_thread_create(
          cache.name.?,
          &cache.thread,
          #{harness(nif.name)},
          @ptrCast(*anyopaque, cache),
          null)) {

        return beam.make_ok_term(env, cache_ref);
      } else return beam.ThreadError.LaunchError;
    }
    """
  end

  def harness_fn(nif) do
    result_assign = if nif.retval == "void", do: "", else: "var result = "

    get_clauses =
      if nif.arity != 0, do: Adapter.get_clauses(nif, &bail/1, &"cache.args.?[#{&1}]"), else: ""

    result_clause =
      case nif.retval do
        "!" <> _type ->
          {r_used?, result_term} =
            Adapter.make_clause(nif.retval, "__r", "cache.yield_info.environment")

          capture_name =
            if nif.arity != 0 and r_used?,
              do: "__r",
              else: "_"

          """
          if (result) | #{capture_name} |
            beam.make_ok_term(
              env,
              e.enif_make_tuple(
                env,
                2,
                cache.this,
                #{result_term}
              )
            )
          else | __e |
            beam.make_error_term(
              env,
              e.enif_make_tuple(
                env,
                2,
                cache.this,
                beam.make_exception(
                  env,
                  "#{nif.module}.ZigError",
                  __e,
                  @errorReturnTrace())
              )
            );
          """

        _ ->
          {_result_used?, result_term} =
            Adapter.make_clause(nif.retval, "result", "cache.yield_info.environment")

          """
          beam.make_ok_term(
              env,
              e.enif_make_tuple(
                env,
                2,
                cache.this,
                #{result_term}
              )
            );
          """
      end

    """
    export fn #{harness(nif.name)}(cache_q: ?*anyopaque) ?*anyopaque {
      var cache: *#{cache(nif.name)} =
        @ptrCast(*#{cache(nif.name)},
          @alignCast(@alignOf(#{cache(nif.name)}), cache_q.?));

      // make sure yield_info is clear.
      // std.debug.assert(beam.yield_info == null);

      // set the threadlocal yield_info.
      // ownership is passed to this function.
      beam.yield_info = &cache.yield_info;

      const env = cache.yield_info.environment;
      defer e.enif_clear_env(env);

      var result_term: beam.term = undefined;

      defer _ = beam.send_advanced(
        null,
        cache.yield_info.parent,
        env,
        result_term
      );

      beam.set_threaded_self();

    #{get_clauses}  // execute the nif function
      #{result_assign}nosuspend #{nif.name}(#{Adapter.args(nif)});
      result_term = #{result_clause}

      // probably unnecessary, but do it anyways.
      beam.yield_info = null;

      // signal that the thread has completed
      if (@atomicRmw(beam.YieldState, &cache.yield_info.state, .Xchg, .Finished, .Monotonic) == .Abandoned) {
        beam.allocator.destroy(cache);
      }

      return null;
    }
    """
  end

  defp bail(:oom),
    do: """
    {
          result_term =
            beam.make_error_term(env,
              e.enif_make_tuple(
                cache.yield_info.environment,
                2,
                cache.this,
                beam.make_atom(env, "enomem"[0..])
              )
            );
          return null;
        }
    """

  defp bail(:function_clause),
    do: """
    {
          result_term =
            beam.make_error_term(env,
              e.enif_make_tuple(
                cache.yield_info.environment,
                2,
                cache.this,
                beam.make_atom(env, "function_clause"[0..])
              )
            );
          return null;
        }
    """

  @impl true
  def zig_adapter(nif, _module) do
    [cache_struct(nif), "\n", launcher_fn(nif), "\n", packer_fn(nif), "\n", harness_fn(nif)]
  end

  @impl true
  def nif_table_entries(nif) do
    """
      e.ErlNifFunc{
        .name = "#{launcher(nif.name)}",
        .arity = #{nif.arity},
        .fptr = #{launcher(nif.name)},
        .flags = 0,
      },
    """
  end
end
