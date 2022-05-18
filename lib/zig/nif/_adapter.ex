defmodule Zig.Nif.Adapter do
  @moduledoc false

  # helper functions for Zigler code adapters as well as a behaviour api describing
  # how they work.

  alias Zig.Parser.Nif

  @doc "converts a Nif struct into zig code that wraps the function call"
  @callback zig_adapter(Nif.t(), module) :: iodata
  @doc "converts a Nif struct into the entries that winds up in the nif table"
  @callback nif_table_entries(Nif.t()) :: iodata
  @doc "elixir-side code that wraps the function call"
  @callback beam_adapter(Nif.t()) :: Macro.t()

  def shim_name(nif_name), do: nif_name |> Atom.to_string() |> String.split(".") |> List.last()

  @env_types ["beam.env", "?*e.ErlNifEnv"]

  def args(%{arity: 0, args: [p]}) when p in @env_types, do: "env"
  def args(%{arity: 0}), do: ""

  def args(nif = %{args: [env | rest]}) when env in @env_types do
    ["env, ", args(%{nif | args: rest})]
  end

  def args(nif) do
    0..(nif.arity - 1)
    |> Enum.map(&"__#{nif.name}_arg#{&1}__")
    |> Enum.join(", ")
  end

  @type bail_reasons :: :oom | :function_clause
  @spec get_clauses(Nif.t(), (bail_reasons -> String.t()), (non_neg_integer -> String.t())) ::
          iodata
  @doc """
  obtains the clauses that convert one type of value into another.

  the second parameter should be code which executed when the argument assignment
  has to bail.  This is context-dependent.

  the third parameter should be a lambda which describes as a string, how one
  fetches the the index to be converted from a term into a proper value.
  """
  def get_clauses(%{arity: 0}, _, _), do: ""

  def get_clauses(%{args: args, name: name}, bail, fetcher) do
    get_clauses(args, name, bail, fetcher)
  end

  defp get_clauses([env | rest], name, bail, fetcher) when env in @env_types do
    get_clauses(rest, name, bail, fetcher)
  end

  defp get_clauses(args, name, bail, fetcher) do
    [
      args
      |> Enum.with_index()
      |> Enum.map(&get_clause(&1, name, bail, fetcher)),
      "\n"
    ]
  end

  defp get_clause({term, index}, function, _bail, fetcher)
       when term in ["beam.term", "e.ErlNifTerm"] do
    "  var __#{function}_arg#{index}__ = #{fetcher.(index)};\n"
  end

  defp get_clause({"[]u8", index}, function, bail, fetcher) do
    ## NB: we don't deallocate strings because the BEAM returns a pointer to memory space that it owns.
    """
      var __#{function}_arg#{index}__ = beam.get_char_slice(env, #{fetcher.(index)})
        catch #{String.trim(bail.(:function_clause))};
    """
  end

  defp get_clause({"[]" <> type, index}, function, bail, fetcher) do
    """
      var __#{function}_arg#{index}__ = beam.get_slice_of(#{short_name(type)}, env, #{
      fetcher.(index)
    }) catch |err| switch (err) {
        error.OutOfMemory => #{String.trim(bail.(:oom))},
        beam.Error.FunctionClauseError => #{String.trim(bail.(:function_clause))}
      };
      defer beam.allocator.free(__#{function}_arg#{index}__);
    """
  end

  defp get_clause({type, index}, function, bail, fetcher) do
    """
      var __#{function}_arg#{index}__ = beam.get_#{short_name(type)}(env, #{fetcher.(index)})
        catch #{String.trim(bail.(:function_clause))};
    """
  end

  def make_clause(type, var, env \\ "env")
  def make_clause("beam.term", var, _), do: var

  def make_clause("void", _var, env) do
    "beam.make_ok(#{env})"
  end

  def make_clause("[]u8", var, env) do
    "beam.make_slice(#{env}, #{var})"
  end

  def make_clause("[]" <> type, var, env) do
    "beam.make_#{type}_list(#{env}, #{var}) catch return beam.raise_enomem(env)"
  end

  def make_clause("!" <> type, var, env) do
    make_clause(type, var, env)
  end

  def make_clause(type, var, env) do
    "beam.make_#{short_name(type)}(#{env}, #{var})"
  end

  defp short_name("beam.pid"), do: "pid"
  defp short_name("e.ErlNifPid"), do: "pid"
  defp short_name(any), do: any
end
