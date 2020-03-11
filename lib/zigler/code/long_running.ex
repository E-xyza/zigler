defmodule Zigler.Code.LongRunning do
  @moduledoc """
  Generates code for long-running nifs.

  long-running functions require several parts to get right.

  0. a zig struct needs to be that holds arguments relvant to the
  long-running nif, these arguments need to be cleaned up in a sane
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

  4. a `fetching` function which is passed the resource struct, and
  marshals it back into BEAM terms, returning the result.  This function
  must be a nif function, as it will be called from the BEAM.

  5. a `cleanup` function which is responsible for cleaning up the
  resource object once the thread has completed its task.
  """

  alias Zigler.Parser.Nif

  #############################################################################
  ## Elixir Metaprogramming for long functions

  def function_skeleton(nif = %Nif{}) do
    quote context: Elixir do
      unquote(long_main_fn(nif))
      unquote(long_launch_fn(nif))
      unquote(long_fetch_fn(nif))
    end
  end

  defp long_main_fn(%{name: name, arity: arity, retval: retval}) do
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

    block = if retval == "void" do
      quote context: Elixir do
        resource = unquote(launcher_call)
        receive do {:done, ^resource} -> :ok end
      end
    else
      quote context: Elixir do
        resource = unquote(launcher_call)
        receive do {:done, ^resource} -> :ok end
        unquote(fetcher name)(resource)
      end
    end

    {:def, [context: Elixir, import: Kernel],
      [
        {name, [context: Elixir], args},
        [do: block]
      ]}
  end

  defp long_launch_fn(%{name: name, arity: arity}) do
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

  defp long_fetch_fn(%{name: name, arity: arity}) do
    text = "nif fetcher for function #{name}/#{arity} not bound"
    quote context: Elixir do
      def unquote(fetcher name)(_) do
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
  def launcher(fn_name), do: String.to_atom("__#{fn_name}_launch__")
  def harness(fn_name), do: String.to_atom("__#{fn_name}_harness__")
  def fetcher(fn_name), do: String.to_atom("__#{fn_name}_fetch__")

  def cache_struct(nif) do
    extra_lines = nif.args
    |> Enum.with_index
    |> Enum.map(fn {type, idx} -> "  arg#{idx}: #{type},\n" end)
    |> Enum.join

    result = if nif.retval == "void", do: "", else: "  result: #{nif.retval}\n"

    """
    const #{cache nif.name} = struct {
      env: beam.env,
      self: beam.pid,
      thread: *std.Thread,
      response: beam.term,
    #{extra_lines}#{result}};

    /// resource: #{cache_ptr nif.name} definition
    const #{cache_ptr nif.name} = ?*#{cache nif.name};

    /// resource: #{cache_ptr nif.name} cleanup
    fn #{cache_cleanup nif.name}(env: beam.env, cache_res_ptr: *#{cache_ptr nif.name}) void {
      if (cache_res_ptr.*) | cache_ptr | {
        beam.allocator.destroy(cache_ptr);
      }
    }
    """
  end

  def launcher_fn(nif) do
    """
    extern fn #{launcher nif.name}(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
      return #{packer nif.name}(env, argv)
        catch beam.raise(env, beam.make_atom(env, "error"));
    }
    """
  end

  def packer_fn(nif) do
    """
    fn #{packer nif.name}(env: beam.env, argv: [*c] const beam.term) !beam.term {
      var cache_term = try __resource__.create(#{cache_ptr nif.name}, env, null);
      errdefer __resource__.release(#{cache_ptr nif.name}, env, cache_term);

      var cache = try beam.allocator.create(#{cache nif.name});
      try __resource__.update(#{cache_ptr nif.name}, env, cache_term, cache);

      var done_atom = beam.make_atom(env, "done");

      cache.env = env;
      cache.self = try beam.self(env);
      cache.response = e.enif_make_tuple(env, 2, done_atom, cache_term);

    #{get_clauses nif}  cache.thread = try std.Thread.spawn(cache, #{harness nif.name});

      return cache_term;
    }
    """
  end

  def harness_fn(nif) do
    cache_args = if nif.args == [] do
      ""
    else
      0..(length(nif.args) - 1)
      |> Enum.map(&"cache.arg#{&1}")
      |> Enum.join(", ")
    end
    result = if nif.retval == "void", do: "", else: "cache.result = "

    """
    fn #{harness nif.name}(cache: *#{cache nif.name}) void {
      #{result}#{nif.name}(#{cache_args});
      var _sent = beam.send(null, cache.self, null, cache.response);
    }
    """
  end

  def fetcher_fn(nif = %{retval: "void"}) do
    """
    extern fn #{fetcher nif.name}(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
      __resource__.release(#{cache_ptr nif.name}, env, argv[0]);
      return beam.make_atom(env, "nil");
    }
    """
  end
  def fetcher_fn(nif) do
    """
    extern fn #{fetcher nif.name}(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {
      var cache_q: ?*#{cache nif.name} = __resource__.fetch(#{cache_ptr nif.name}, env, argv[0])
        catch return beam.raise_function_clause_error(env);
      defer __resource__.release(#{cache_ptr nif.name}, env, argv[0]);

      if (cache_q) | cache | {
        return #{make_clause nif.retval, "cache.result"};
      } else {
        return beam.raise_function_clause_error(env);
      }
    }
    """
  end

  def adapter(nif) do
    [cache_struct(nif), "\n",
     launcher_fn(nif), "\n",
     packer_fn(nif), "\n",
     harness_fn(nif), "\n",
     fetcher_fn(nif)]
  end

  @env_types ["beam.env", "?*e.ErlNifEnv"]

  defp get_clauses(%{arity: 0}), do: ""
  defp get_clauses(%{args: args}), do: get_clauses(args)

  defp get_clauses([env | rest]) when env in @env_types, do: get_clauses(rest)
  defp get_clauses([]), do: []
  defp get_clauses(args) do
    [args
    |> Enum.with_index
    |> Enum.map(&get_clause/1), "\n"]
  end

  defp get_clause({term, index}) when term in ["beam.term", "e.ErlNifTerm"] do
    """
      cache.arg#{index} = argv[#{index}];
    """
  end
  defp get_clause({"[]u8", index}) do
    """
      cache.arg#{index} = try beam.get_char_slice(env, argv[#{index}]);
    """
  end
  defp get_clause({"[]" <> type, index}) do
    """
      cache.arg#{index} = try beam.get_slice_of(#{short_name type}, env, argv[#{index}]);
    """
  end
  defp get_clause({type, index}) do
    """
      cache.arg#{index} = try beam.get_#{short_name type}(env, argv[#{index}]);
    """
  end

  defp short_name("beam.pid"), do: "pid"
  defp short_name("e.ErlNifPid"), do: "pid"
  defp short_name(any), do: any

  defp make_clause("[]u8", var) do
    "beam.make_slice(env, #{var})"
  end
  defp make_clause("[]" <> type, var) do
    "beam.make_#{type}_list(env, #{var}) catch return beam.raise_enomem(env)"
  end
  defp make_clause(type, var) do
    "beam.make_#{short_name type}(env, #{var})"
  end
end
