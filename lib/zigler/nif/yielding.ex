defmodule Zigler.Nif.Yielding do
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

  alias Zigler.Parser.Nif
  alias Zigler.Typespec
  alias Zigler.Nif.Adapter

  @behaviour Adapter

  #############################################################################
  ## Elixir Metaprogramming for yielding functions

  @impl true
  def beam_adapter(nif) do
    typespec = Typespec.from_nif(nif)
    quote do
      unquote(typespec)
      unquote(basic_fn(nif))
    end
  end

  defp basic_fn(%{name: name, arity: arity}) do
    text = "nif for function #{name}/#{arity} not bound"

    args = if arity == 0 do
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

  def frame_ptr(fn_name), do: String.to_atom("__#{fn_name}_frame_ptr__")
  def frame(fn_name), do: String.to_atom("beam.Frame(#{fn_name})")
  def frame_cleanup(fn_name), do: String.to_atom("__#{fn_name}_frame_cleanup__")
  def cancel_join(fn_name), do: String.to_atom("__cancel_join_#{fn_name}__")
  def launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  def launcher_shim(fn_name), do: String.to_atom("__#{fn_name}_launcher_shim__")
  def rescheduler(fn_name), do: String.to_atom("__#{fn_name}_rescheduler__")
  def rescheduler_shim(fn_name), do: String.to_atom("__#{fn_name}_rescheduler_shim__")
  def scheduler(fn_name), do: String.to_atom("__#{fn_name}_scheduler__")
  def harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")

  @spec frame_resources(Nif.t) :: iodata
  def frame_resources(nif) do
    """
    /// resource: #{frame_ptr nif.name} definition
    const #{frame_ptr nif.name} = *#{frame(scheduler nif.name)};

    /// resource: #{frame_ptr nif.name} cleanup
    fn #{frame_cleanup nif.name}(env: beam.env, frame_ptr: *#{frame_ptr nif.name}) void {
      _ = async #{cancel_join nif.name}(env, frame_ptr);
    }

    fn #{cancel_join nif.name}(env: beam.env, beam_frame: *#{frame_ptr nif.name}) void {
      // async join with cancellation.
      beam.yield_info = &(beam_frame.*.yield_info);
      beam.yield_info.cancelled = true;

      resume beam_frame.*.zig_frame;
      await beam_frame.*.zig_frame;
    }
    """
  end

  @spec launcher_fns(Nif.t) :: iodata
  def launcher_fns(nif) do
    """
    export fn #{launcher nif.name}(env: beam.env, _argc: c_int, argv: [*c] const beam.term) beam.term {
      return #{launcher_shim nif.name}(env, argv) catch | err | switch (err) {
        error.OutOfMemory => beam.raise_enomem(env),
      };
    }

    fn #{launcher_shim nif.name}(env: beam.env, argv: [*c] const beam.term) !beam.term {
      // first, create a frame on the heap.
      // irl this needs to be resource that we can jam into the rescheduler.
      var beam_frame = try beam.allocator.create(#{frame(scheduler nif.name)});
      errdefer beam.allocator.destroy(beam_frame);

      beam_frame.zig_frame = try beam.allocator.create(@Frame(#{scheduler nif.name}));
      errdefer beam.allocator.destroy(beam_frame.zig_frame);

      var frame_resource = try __resource__.create(#{frame_ptr nif.name}, env, beam_frame);

      beam_frame.yield_info = .{
        .environment = env,
        .self = frame_resource,
        .name = "#{nif.name}",
        .rescheduler = #{rescheduler nif.name},
      };

      // set the threadlocal yield info pointer.
      beam.yield_info = &beam_frame.yield_info;

      // run the desired function.
      beam_frame.zig_frame.* = async #{scheduler nif.name}(env, argv, beam.yield_info);
      return beam_frame.yield_info.response;
    }
    """
  end

  @spec rescheduler_fns(Nif.t) :: iodata
  def rescheduler_fns(nif) do
    """
    /// C abi function that acts as the seam between reentrancy that the BEAM FFI expects and
    /// the zig-style async runner (this is for nif #{nif.name}).
    export fn #{rescheduler nif.name}(env: beam.env, argc: c_int, argv: [*c]const beam.term) beam.term {
      // take out a lease on the resource
      __resource__.keep(#{frame_ptr nif.name}, env, argv[0]) catch return beam.raise_resource_error(env);

      //pull frame information from the passed resource.
      var beam_frame = __resource__.fetch(#{frame_ptr nif.name}, env, argv[0])
        catch return beam.raise_resource_error(env);

      // update the threadlocal yield_info value.
      beam.yield_info = &(beam_frame.*.yield_info);

      // sometimes the environment can shift.  In this case, you have to update the contents of
      // yield_info; and make sure the resource token is known to the new environment.
      // update, as necessary (if the environment has shifted)
      if (beam.yield_info.environment != env) {
        beam.yield_info.environment = env;
        beam.yield_info.self = e.enif_make_copy(env, argv[0]);
      } else {
        // always update the self object id.
        beam.yield_info.self = argv[0];
      }

      // hand-off from C ABI into Zig ABI to allow for async operations.
      _ = async #{rescheduler_shim nif.name}(env, beam_frame);

      // BEAM expects the result of `e.enif_schedule_nif` as an response, if it needs to be
      // rescheduled, or the NIF return value otherwise.
      return beam.yield_info.response;
    }

    fn #{rescheduler_shim nif.name}(env: beam.env, beam_frame: *#{frame(scheduler nif.name)}) void {
      beam_frame.yield_info.yielded = false;

      resume beam_frame.zig_frame;

      if (! beam_frame.yield_info.yielded) {
        await beam_frame.zig_frame;
      }
    }
    """
  end

  @spec scheduler_fn(Nif.t) :: iodata
  def scheduler_fn(nif) do
    """
    fn #{scheduler nif.name}(_env: beam.env, argv: [*c] const beam.term, yield_info: *beam.YieldInfo) void {
      var env = _env;
      var harness_frame = async #{harness nif.name}(env, argv);

      // timekeeping utilities
      var to: i64 = e.enif_monotonic_time(e.ErlNifTimeUnit.ERL_NIF_USEC);
      var tf: i64 = undefined;
      var elapsed: i64 = undefined;
      var percent: c_int = undefined;

      // enter a scheduler loop around the harness frame.
      while (yield_info.yielded) {
        tf = e.enif_monotonic_time(e.ErlNifTimeUnit.ERL_NIF_USEC);
        elapsed = tf - to;
        // don't ask the vm for timeslice info too often.
        if (elapsed > 100) {
          percent = @intCast(c_int, @divFloor(elapsed, 10));
          if (e.enif_consume_timeslice(env, percent) == 1) {
            // release the lease on the resource.
            __resource__.release(#{frame_ptr nif.name}, env, yield_info.self);
            // reschedule the yielded function.
            yield_info.response = beam.reschedule();
            // forward the suspension if we have consumed too many timeslices
            suspend;
            // update the environment, which could have changed.
            env = yield_info.environment;
            to = e.enif_monotonic_time(e.ErlNifTimeUnit.ERL_NIF_USEC);
          } else {
            to = tf;
          }
        }
        // flag as not yielded yet.
        yield_info.yielded = false;

        resume harness_frame;
      }
      return;
    }
    """
  end

  @spec harness_fns(Nif.t) :: iodata
  def harness_fns(nif) do
    get_clauses = Adapter.get_clauses(nif, &bail/1, &"argv[#{&1}]")
    result_term = if nif.retval == "void" do
      """
      beam.make_ok(env);
      await nif_frame
      """
    else
      Adapter.make_clause(nif.retval, "await nif_frame", "beam.yield_info.environment")
    end
    """
    fn #{harness nif.name}(env_: beam.env, argv: [*c] const beam.term) void {
      var env = env_;

      // decode parameters
    #{get_clauses}

      // launch the nif frame.
      var nif_frame = async #{nif.name}(#{Adapter.args nif});
      while (beam.yield_info.yielded) {
        suspend;
        resume nif_frame;
      }

      // join the result, since it finished.
      beam.yield_info.response = #{result_term};
    }
    """
  end

  def bail(:oom), do: """
  {
        beam.yield_info.response = beam.raise_enomem(env);
        return;
      }
  """
  def bail(:function_clause), do: """
  {
        beam.yield_info.response = beam.raise_function_clause_error(env);
        return;
      }
  """

  @impl true
  def zig_adapter(nif) do
    [frame_resources(nif), "\n",
     launcher_fns(nif), "\n",
     rescheduler_fns(nif), "\n",
     scheduler_fn(nif), "\n",
     harness_fns(nif)]
  end

  @impl true
  def nif_table_entries(nif) do
    """
      e.ErlNifFunc{
        .name = "#{nif.name}",
        .arity = #{nif.arity},
        .fptr = #{launcher nif.name},
        .flags = 0,
      },
    """
  end
end
