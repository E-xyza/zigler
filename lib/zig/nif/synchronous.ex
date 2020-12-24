defmodule Zig.Nif.Synchronous do

  @moduledoc """
  Adapter code for synchronous nifs
  """

  alias Zig.Nif.Adapter
  alias Zig.Typespec

  @behaviour Adapter

  @impl true
  def zig_adapter(nif) do
    get_clauses = Adapter.get_clauses(nif, &bail/1, &"argv[#{&1}]")

    head = "export fn __#{nif.name}_shim__(env: beam.env, argc: c_int, argv: [*c] const beam.term) beam.term {"

    result = cond do
      nif.retval in ["beam.term", "e.ErlNifTerm", "!beam.term", "!e.ErlNifTerm"] ->
        """
          return #{function_call nif}}
        """
      nif.retval in ["void", "!void"] ->
        """
          #{function_call nif}  return beam.make_nil(env);
        }
        """
      true ->
        """
          var #{result_var nif} = #{function_call nif}  return #{Adapter.make_clause nif.retval, result_var(nif)};
        }
        """
    end
    [head, "\n", get_clauses, result, "\n"]
  end

  defp bail(:oom), do: "return beam.raise_enomem(env)"
  defp bail(:function_clause), do: "return beam.raise_function_clause_error(env)"

  defp result_var(nif), do: "__#{nif.name}_result__"

  # error return calls.
  defp function_call(nif = %{retval: "!" <> _}) do
    """
    #{nif.name}(#{Adapter.args nif}) catch {
      return beam.make_error_term(env, beam.make_error_return_trace(env, @errorReturnTrace()));
    };
    """
  end
  defp function_call(nif), do: "#{nif.name}(#{Adapter.args nif});\n"

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

end
