defmodule Zig.Nif.Synchronous do
  @moduledoc """
  Adapter code for synchronous nifs
  """

  alias Zig.Nif.Adapter
  alias Zig.Typespec

  @behaviour Adapter

  @impl true
  def zig_adapter(nif, module) do
    get_clauses = Adapter.get_clauses(nif, &bail/1, &"argv[#{&1}]")
    env = if Adapter.uses_env?(nif), do: "env", else: "_"
    argv = if Adapter.uses_argv?(nif), do: "argv", else: "_"

    head = """
    export fn __#{nif.name}_shim__(#{env}: beam.env, _: c_int, #{argv}: [*c] const beam.term) beam.term {
      beam.yield_info = null;
    """

    result =
      case nif.retval do
        v when v in ["beam.term", "e.ErlNifTerm", "!beam.term", "!e.ErlNifTerm"] ->
          """
            return nosuspend #{function_call(nif, module)}}
          """

        v when v in ["void", "!void"] ->
          """
            nosuspend #{function_call(nif, module)}  return beam.make_ok(env);
          }
          """

        other ->
          {_used?, result_var} = Adapter.make_clause(other, result_var(nif))

          """
            var #{result_var(nif)} = nosuspend #{function_call(nif, module)}  return #{result_var};
          }
          """
      end

    [head, "\n", get_clauses, result, "\n"]
  end

  defp bail(:oom), do: "return beam.raise_enomem(env)"
  defp bail(:function_clause), do: "return beam.raise_function_clause_error(env)"

  defp result_var(nif), do: "__#{nif.name}_result__"

  # error return calls.
  defp function_call(nif = %{retval: "!" <> _}, module) do
    """
    #{nif.name}(#{Adapter.args(nif)}) catch |err| {
      return beam.raise_exception(env, "#{module}.ZigError", err, @errorReturnTrace());
    };
    """
  end

  defp function_call(nif, _module), do: "#{nif.name}(#{Adapter.args(nif)});\n"

  @impl true
  def nif_table_entries(nif) do
    """
      e.ErlNifFunc{
        .name = "#{nif.name}",
        .arity = #{nif.arity},
        .fptr = __#{nif.name}_shim__,
        .flags = 0,
      },
    """
  end

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

    args =
      if arity == 0 do
        Elixir
      else
        for _ <- 1..arity, do: {:_, [], Elixir}
      end

    body =
      quote context: Elixir do
        raise unquote(text)
      end

    {:def, [context: Elixir, import: Kernel],
     [
       {name, [context: Elixir], args},
       [do: body]
     ]}
  end
end
