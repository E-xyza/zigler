defmodule Zigler.LongRunning do

  @moduledoc """
  long-running functions require several parts to get right.

  Zero, a zig struct needs to be defined with critical BEAM resource
  parameters.  This struct is going to be parametric on the nif parameters.

  First, a `packer` function which takes the beam parameters and shoves
  them into a zig struct to be stored in a BEAM resource, then launches
  the function, returning the resource.

  Second, a `launcher` function which runs the `packer` wrapping the errors
  from the launch function.

  Third, a `harness` function which is passed the resource struct, and is
  responsible for updating the resource, sending back a "finished"
  message to parent context, and cleaning up the resource struct (but not
  the resource itself).

  Fourth, a `fetching` function which is passed the resource struct, and
  marshals it back into BEAM terms, returning the result.

  Fifth, a `destructor` function which the BEAM itself calls to free up
  any memory allocated by the packager.
  """

  defmodule Zig do
    @moduledoc """
    zig code generators for long-running functions.
    """

    defp resource(fn_name), do: "#{fn_name}_resource"
    defp cache(fn_name), do: "__#{fn_name}_cache__"
    defp packer(fn_name), do: "__#{fn_name}_packer__"
    defp launcher(fn_name), do: "__launch_#{fn_name}__"
    defp harness(fn_name), do: "__#{fn_name}_harness__"
    defp fetcher(fn_name), do: "__fetch_#{fn_name}__"

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

  def functions(nif) do
    []
  end
end
