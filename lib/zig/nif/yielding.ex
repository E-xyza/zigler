defmodule Zig.Nif.Yielding do
  @moduledoc """
  Generates code for yielding nifs.

  yielding functions require several parts to get right.

  0. a zig struct that holds the frame of the async function and also
  information to be filled in for the yielding allocator.

  1. a `launcher` function which serves as the interface between the C abi,
  which cannot tolerate async functions and zig function that is going to be
  called.

  2. a `supervisor` function which is responsible for resuming the async
  function.  This is the event loop, but it uses a tail-call sort of a system
  that Erlang is used to.

  3. a `harness` function which takes care of informing the supervisor that
  target function has been completed.
  """

  alias Zig.Nif.Adapter
  alias Zig.Parser.Nif
  alias Zig.Typespec

  @behaviour Adapter

  #############################################################################
  ## Elixir Metaprogramming for yielding functions

  @impl true
  def beam_adapter(nif) do
    typespec = Typespec.from_nif(nif)

    quote context: Elixir do
      unquote(typespec)
      unquote(basic_fn(nif))
    end
  end

  defp basic_fn(%{name: name, arity: arity}) do
    text = "nif for function #{name}/#{arity} not bound"

    args =
      if arity == 0 do
        Elixir
      else
        for _ <- 1..arity, do: {:_, [], Elixir}
      end

    {:def, [context: Elixir, import: Kernel],
     [
       {name, [context: Elixir], args},
       [do: {:raise, [context: Elixir, import: Kernel], [text]}]
     ]}
  end

  #############################################################################
  ## Zig metaprogramming

  def frame_type(fn_name), do: String.to_atom("__#{fn_name}_frame__")
  def frame_ptr(fn_name), do: String.to_atom("__#{fn_name}_frame_ptr__")
  def frame_cleanup(fn_name), do: String.to_atom("__#{fn_name}_frame_cleanup__")

  def cancel_join(fn_name), do: String.to_atom("__cancel_join_#{fn_name}__")
  def launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  def launcher_shim(fn_name), do: String.to_atom("__#{fn_name}_launcher_shim__")
  def rescheduler(fn_name), do: String.to_atom("__#{fn_name}_rescheduler__")
  def harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")

  @spec frame_resources(Nif.t()) :: iodata
  def frame_resources(nif) do
    """
    /// resource: #{frame_ptr(nif.name)} definition
    const #{frame_ptr(nif.name)} = *beam.Frame(#{harness(nif.name)});

    /// resource: #{frame_type(nif.name)} cleanup
    fn #{frame_cleanup(nif.name)}(_: beam.env, beam_frame_ptr: *#{frame_ptr(nif.name)}) void {
      const allocator = beam.large_allocator;

      var beam_frame = beam_frame_ptr.*;

      // only join the frame if it's still in the "yielded" state
      if (beam_frame.yield_info.yield_frame) | yield_frame | {
        // async join with cancellation.
        beam.yield_info = &(beam_frame.yield_info);
        beam.yield_info.?.state = .Cancelled;

        resume yield_frame;
        nosuspend await beam_frame.zig_frame;
      }

      // always destroy the beam environment for the thread
      e.enif_free_env(beam_frame.yield_info.environment);

      defer allocator.destroy(beam_frame);
      defer allocator.destroy(beam_frame.zig_frame);

      // sometimes a little shuteye goes a long way.  Note this is in ns.
      // for some reason, this prevents a segfault.
      std.time.sleep(100);
    }
    """
  end

  @spec launcher_fns(Nif.t()) :: iodata
  def launcher_fns(nif) do
    """
    export fn #{launcher(nif.name)}(env: beam.env, _: c_int, argv: [*c] const beam.term) beam.term {
      return #{launcher_shim(nif.name)}(env, argv) catch | err | switch (err) {
        error.LaunchError => beam.raise(env, beam.make_atom(env, "launch_error")),
        error.OutOfMemory => beam.raise_enomem(env),
        error.Cancelled => unreachable,
      };
    }

    fn #{launcher_shim(nif.name)}(env: beam.env, argv: [*c] const beam.term) !beam.term {
      const allocator = beam.large_allocator;

      // first, create the beam.Frame on the heap.  this needs to be resource that we can jam
      // into the rescheduler tail-call's argument list.
      var beam_frame = try allocator.create(beam.Frame(#{harness(nif.name)}));
      errdefer allocator.destroy(beam_frame);

      // next create the zig frame for the scheduler on the heap.
      beam_frame.zig_frame = try allocator.create(@Frame(#{harness(nif.name)}));
      errdefer allocator.destroy(beam_frame.zig_frame);

      var frame_resource = try __resource__.create(#{frame_ptr(nif.name)}, env, beam_frame);

      // create a new environment for the yielding nif.
      var yield_env = e.enif_alloc_env() orelse return beam.YieldError.LaunchError;

      // populate initial information for the yield clause
      beam_frame.yield_info = .{ .environment = yield_env };

      // set the threadlocal yield info pointer.
      beam.yield_info = &(beam_frame.yield_info);

      // run the desired function.
      beam_frame.zig_frame.* = async #{harness(nif.name)}(yield_env, env, argv);

      // mark the resource for releasing here.
      __resource__.release(#{frame_ptr(nif.name)}, env, frame_resource);

      var exception_reason: beam.term = undefined;
      if (e.enif_has_pending_exception(yield_env, &exception_reason) == 1) {
        // Propagate exceptions from yield_env
        return e.enif_raise_exception(env, exception_reason);
      }

      if (beam.yield_info.?.yield_frame) | _ | {
        return e.enif_schedule_nif(env, "#{nif.name}", 0, #{rescheduler(nif.name)}, 1, &frame_resource);
      } else {

        // The response was created using yield_env, so it has to be copied back in the process env
        // TODO: this does not cover lists and maps, because enif_make_copy will just take care
        // of copying the outer term but the inner terms will still point to the old env
        const __response = e.enif_make_copy(env, beam_frame.yield_info.response);

        if (beam_frame.yield_info.errored) {
          return e.enif_raise_exception(env, __response);
        } else {
          return __response;
        }
      }
    }
    """
  end

  @spec rescheduler_fn(Nif.t()) :: iodata
  @doc """
  the rescheduler fn is a seam between the tail-call reentrancy of the BEAM FFI.
  """
  def rescheduler_fn(nif) do
    """
    export fn #{rescheduler(nif.name)}(env: beam.env, _: c_int, argv: [*c] const beam.term) beam.term {
      var beam_frame = __resource__.fetch(#{frame_ptr(nif.name)}, env, argv[0]) catch
        return beam.raise_resource_error(env);

      var start_time = e.enif_monotonic_time(e.ERL_NIF_USEC);
      var tick_time: e.ErlNifTime = undefined;
      var elapsed_time: e.ErlNifTime = undefined;

      // reset the threadlocal yield_info pointer.
      beam.yield_info = &(beam_frame.yield_info);

      while (beam.yield_info.?.yield_frame) |next_frame| {
        // stash the yielding frame and resume it:
        beam.yield_info.?.yield_frame = null;
        resume next_frame;
        tick_time = e.enif_monotonic_time(e.ERL_NIF_USEC);
        elapsed_time = tick_time - start_time;

        if (elapsed_time >= 100) {
          // seems to be necessary to prevent segfaults.  Note this is in ns.  Seems to work
          // on @ityonemo platform with 1ns but expanded to 10 in case slower platforms
          // still have problems.
          std.time.sleep(10);
          if (e.enif_consume_timeslice(env, @intCast(c_int, @divFloor(elapsed_time, 10))) == 0) {
            start_time = tick_time;
          } else break;
        }
      } else {
        nosuspend await beam_frame.zig_frame;
        return beam.yield_info.?.response;
      }

      return e.enif_schedule_nif(env, "#{nif.name}", 0, #{rescheduler(nif.name)}, 1, argv);
    }
    """
  end

  @spec harness_fns(Nif.t()) :: iodata
  def harness_fns(nif) do
    get_clauses = Adapter.get_clauses(nif, &bail/1, &"argv[#{&1}]")

    argv = if Adapter.uses_argv?(nif), do: "argv", else: "_"

    result_term =
      case nif.retval do
        "void" ->
          "beam.make_ok(env); _ = result"

        "!" <> _ ->
          {_r_used?, r} = Adapter.make_clause(nif.retval, "__r", "beam.yield_info.?.environment")

          """
          if (result) | __r |
            #{r}
          else | __e | res: {
            beam.yield_info.?.errored = true;
            break :res beam.make_exception(
              beam.yield_info.?.environment,
              "#{nif.module}.ZigError",
              __e,
              @errorReturnTrace());
          }

          """

        _ ->
          {_result_used?, result} =
            Adapter.make_clause(nif.retval, "result", "beam.yield_info.?.environment")

          result
      end

    """
    fn #{harness(nif.name)}(env: beam.env, parent_env: beam.env, #{argv}: [*c] const beam.term) callconv(.Async) void {
      _ = env;
      _ = parent_env;
      // decode parameters
    #{get_clauses}

      // launch the nif frame.
      const result = #{nif.name}(#{Adapter.args(nif)});

      const result_term = #{result_term};
      // join the result, since it finished.
      beam.set_yield_response(result_term);
    }
    """
  end

  def bail(:oom),
    do: """
    {
          beam.yield_info.?.response = beam.raise_enomem(parent_env);
          return;
        }
    """

  def bail(:function_clause),
    do: """
    {
          beam.yield_info.?.response = beam.raise_function_clause_error(parent_env);
          return;
        }
    """

  @impl true
  def zig_adapter(nif, _module) do
    [
      frame_resources(nif),
      "\n",
      launcher_fns(nif),
      "\n",
      rescheduler_fn(nif),
      "\n",
      harness_fns(nif)
    ]
  end

  @impl true
  def nif_table_entries(nif) do
    """
      e.ErlNifFunc{
        .name = "#{nif.name}",
        .arity = #{nif.arity},
        .fptr = #{launcher(nif.name)},
        .flags = 0,
      },
    """
  end
end
