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
  def launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  def rescheduler(fn_name), do: String.to_atom("__#{fn_name}_rescheduler__")
  def rescheduler_shim(fn_name), do: String.to_atom("__#{fn_name}_rescheduler_shim__")
  def joiner(fn_name), do: String.to_atom("__#{fn_name}_joiner__")
  def harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")

  @env_types ["beam.env", "?*e.ErlNifEnv"]

  def frame_struct(nif) do
    """
    /// resource: #{frame_ptr nif.name} definition
    const #{frame_ptr nif.name} = *#{frame(harness nif.name)};

    /// resource: #{frame_ptr nif.name} cleanup
    fn #{frame_cleanup nif.name}(env: beam.env, frame_res_ptr: *#{frame_ptr nif.name}) void {
      // do nothing, for now.
    }
    """
  end

  def launcher_fn(nif) do
    """
    export fn #{launcher nif.name}(env: beam.env, _argc: c_int, argv: [*c] const beam.term) beam.term {
      // first, create a frame on the heap.
      // irl this needs to be resource that we can jam into the rescheduler.
      var beam_frame = beam.allocator.create(#{frame(harness nif.name)}) catch unreachable;
      // put in errdefer here.
      beam_frame.zig_frame = beam.allocator.create(@Frame(#{harness nif.name})) catch unreachable;
      // put in errdefer here

      var frame_resource = __resource__.create(#{frame_ptr nif.name}, env, beam_frame)
        catch unreachable;

      // lock down the resource
      __resource__.keep(#{frame_ptr nif.name}, env, frame_resource) catch unreachable;

      beam_frame.yield_info = .{
        .environment = env,
        .self = frame_resource,
        .name = "#{nif.name}"[0..],
        .rescheduler = #{rescheduler nif.name},
      };

      // set the threadlocal yield info pointer.
      beam.yield_info = &beam_frame.yield_info;

      // run the desired function.
      beam_frame.zig_frame.* = async #{harness nif.name}(env, argv, beam.yield_info);

      return beam_frame.yield_info.response;
    }
    """
  end

  def rescheduler_fns(nif) do
    """
    /// C abi function that acts as the seam between reentrancy that the BEAM FFI expects and
    /// the zig-style async runner (this is for nif #{nif.name}).
    export fn #{rescheduler nif.name}(env: beam.env, argc: c_int, argv: [*c]const beam.term) beam.term {
      //pull frame information from the passed resource.
      var beam_frame = __resource__.fetch(#{frame_ptr nif.name}, env, argv[0]) catch unreachable;

      // sometimes the environment can shift.  In this case, you have to update the contents of
      // yield_info; and make sure the resource token is known to the new environment.
      // update, as necessary (if the environment has shifted)
      if (beam_frame.*.yield_info.environment != env) {
        beam_frame.*.yield_info.environment = env;
        beam_frame.*.yield_info.self = e.enif_make_copy(env, argv[0]);
      }

      // hand-off from C ABI into Zig ABI to allow for async operations.
      _ = async #{rescheduler_shim nif.name}(env, beam_frame);

      // BEAM expects the result of `e.enif_schedule_nif` as an response, if it needs to be
      // rescheduled, or the NIF return value otherwise.
      return beam_frame.*.yield_info.response;
    }

    fn #{rescheduler_shim nif.name}(env: beam.env, beam_frame: *#{frame(harness nif.name)}) void {
      beam_frame.yield_info.yielded = false;
      beam.yielding_allocator = beam_frame.yielding_allocator;

      resume beam_frame.zig_frame;

      if (! beam_frame.yield_info.yielded) {
        await beam_frame.zig_frame;
      }
    }
    """
  end

  def joiner_fn(nif) do
    result_term = if nif.retval == "void" do
      "beam.make_ok(cache.env)"
    else
      Adapter.make_clause(nif.retval, "await beam_frame.zig_frame", "env")
    end

    """
    fn #{joiner nif.name}(env: beam.env, beam_frame: *#{frame(harness nif.name)}) void {
      beam_frame.yield_info.response = #{result_term};
    }
    """
  end

  def harness_fn(nif) do
    get_clauses = Adapter.get_clauses(nif, &bail/1, &"argv[#{&1}]")
    """
    fn #{harness nif.name}(env: beam.env, argv: [*c] const beam.term, yield_info: *beam.YieldInfo) void {
      // decode parameters
    #{get_clauses}

      var to: i64 = e.enif_monotonic_time(e.ErlNifTimeUnit.ERL_NIF_USEC);
      var tf: i64 = undefined;
      var percent: c_int = undefined;

      // launch the inner frame.
      var inner_frame = async #{nif.name}(#{Adapter.args nif});

      // go into a scheduler loop around the inner frame.
      while (yield_info.yielded) {
        tf = e.enif_monotonic_time(e.ErlNifTimeUnit.ERL_NIF_USEC);

        // 10 microseconds in one percent of a millisecond.
        percent = @intCast(c_int, @divFloor(tf - to, 10));
        if (e.enif_consume_timeslice(env, percent) == 1) {
          // reschedule the yielded function.
          yield_info.response = e.enif_schedule_nif(
            yield_info.environment,
            yield_info.name.ptr,
            0,
            yield_info.rescheduler,
            1,
            &yield_info.self);

          // forward the suspension if we have consumed too many timeslices
          suspend;
          to = e.enif_monotonic_time(e.ErlNifTimeUnit.ERL_NIF_USEC);
        } else {
          to = tf;
        }

        // update beam.yield info to make sure we're synced
        beam.yield_info = yield_info;
        // flag as not yielded yet.
        yield_info.yielded = false;

        resume inner_frame;
      }
      // join the result, since it finished.
      yield_info.response = beam.make_i32(env, await inner_frame);
      return;
    }
    """
  end

  def bail(:oom), do: """
  {
        yield_info.response = beam.raise_enomem(env);
        return;
      }
  """
  def bail(:function_clause), do: """
  {
        yield_info.response = beam.raise_function_clause_error(env);
        return;
      }
  """

  @impl true
  def zig_adapter(nif) do
    [frame_struct(nif), "\n",
     launcher_fn(nif), "\n",
     rescheduler_fns(nif), "\n",
     joiner_fn(nif), "\n",
     harness_fn(nif)]
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
