defmodule Zigler.Code.LongRunning do
  @moduledoc """
  Generates code for long-running nifs.

  long-running functions require several parts to get right.

  0. a zig struct needs to be that holds parameters relvant to the
  long-running nif, these parameters need to be cleaned up in a sane
  fashion once the nif has completed running. This struct is going to be
  parametric on the nif parameters, and will be packed into a BEAM
  resource.

  1. a `packer` function which takes the beam parameters and shoves
  them into a zig struct to be stored in a BEAM resource, then launches
  the function, returning the resource.

  2. a `launcher` function which runs the `packer` wrapping the errors
  from the launch function.  The launch function must be a nif function,
  as it will be called from the BEAM.

  3. a `harness` function which is passed the resource struct, and is
  responsible for updating the resource, sending back a "finished"
  message to parent context.  The harness function will be responsible
  for wrapping the declared nif function.

  4. a `fetching` function which is passed the resource struct, and
  marshals it back into BEAM terms, returning the result.  This function
  must be a nif function, as it will be called from the BEAM.

  5. a `cleanup` function which is responsible for cleaning up the
  resource object once the thread has completed its task.
  """

  def resource(fn_name), do: String.to_atom("#{fn_name}_resource")
  def cache(fn_name), do: String.to_atom("__#{fn_name}_cache__")
  def packer(fn_name), do: String.to_atom("__#{fn_name}_pack__")
  def launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  def harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")
  def fetcher(fn_name), do: String.to_atom("__#{fn_name}_fetch__")

  def cache_struct(nif) do
    extra_lines = nif.params
    |> Enum.with_index
    |> Enum.map(fn {type, idx} -> "  v#{idx}: #{type},\n" end)
    |> Enum.join

    """
    const #{cache nif.name} = struct {
      env: beam.env,
      self: beam.pid,
      ref: beam.term,
      thread: *std.Thread,
    #{extra_lines}  res: beam.term
    };
    """
  end

  def packer_fn(nif) do
    variables = nif.params
    |> Enum.with_index
    |> Enum.map(fn {type, idx} -> ", v#{idx}: #{type}" end)
    |> Enum.join()

    cache_lines = if nif.params == [] do
      ""
    else
      0..length(nif.params)-1
      |> Enum.map(&"  cache.v#{&1} = v#{&1};\n")
      |> Enum.join()
    end

    """
    fn #{packer nif.name}(cache_ret: **#{cache nif.name}, env: beam.env#{variables}) !void {

      var cache = try beam.allocator.create(#{cache nif.name});
      errdefer { beam.allocator.destroy(cache); }

      cache.env = env;
      cache.self = try beam.self(env);
      cache.ref = try beam.make_ref(env);
      cache.res = try beam.resource.create(i64, env, #{resource nif.name}, undefined);

    #{cache_lines}
      cache.thread = try std.Thread.spawn(cache, #{harness nif.name});
      cache_ret.* = cache;
    }
    """
  end

  def launcher_fn(nif) do
    params = nif.params
    |> Enum.with_index
    |> Enum.map(fn {type, idx} -> ", v#{idx}: #{type}" end)
    |> Enum.join()

    param_call = if nif.params == [] do
      ""
    else
      0..length(nif.params)-1
      |> Enum.map(&", v#{&1}")
      |> Enum.join()
    end

    """
    fn #{launcher nif.name}(env: beam.env#{params}) beam.term {
      var cache: *#{cache nif.name} = undefined;

      #{packer nif.name}(&cache, env#{param_call}) catch {
        return beam.raise(env, beam.make_atom(env, "error"[0..]));
      };

      return e.enif_make_tuple(env, 2, cache.ref, cache.res);
    }
    """
  end

  def harness_fn(nif) do
    cache_params = if nif.params == [] do
      ""
    else
      0..length(nif.params)-1
      |> Enum.map(&"cache.v#{&1}")
      |> Enum.join(", ")
    end
    """
    fn #{harness nif.name}(cache: *#{cache nif.name}) void {
      defer beam.allocator.destroy(cache);

      var result = #{nif.name}(#{cache_params});

      beam.resource.update(#{nif.retval}, cache.env, #{resource nif.name}, cache.res, result)
        catch |err| return;

      var res = e.enif_send(null, &cache.self, cache.env, cache.ref);
    }
    """
  end

  def fetch_fn(nif) do
    """
    fn #{fetcher nif.name}(env: beam.env, resource: beam.term) beam.term {
      var result = beam.resource.fetch(#{nif.retval}, env, add_resource, resource)
        catch |err| return beam.raise(env, beam.make_atom(env, "resource error"[0..]));

      // release the resource.
      beam.resource.release(env, add_resource, resource);

      return beam.make_i64(env, result);
    }
    """
  end

end
