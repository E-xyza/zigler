defmodule Zigler.Nif.Adapter do
  @moduledoc false

  # helper functions for Zigler code adapters as well as a behaviour api describing
  # how they work.

  alias Zigler.Parser.Nif

  @doc "converts a Nif struct into zig code that wraps the function call"
  @callback zig_adapter(Nif.t) :: iodata
  @doc "converts a Nif struct into the entries that winds up in the nif table"
  @callback nif_table_entries(Nif.t) :: iodata
  @doc "elixir-side code that wraps the function call"
  @callback beam_adapter(Nif.t) :: Macro.t

  def shim_name(nif_name), do: nif_name |> Atom.to_string |> String.split(".") |> List.last

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

  def get_clauses(nif, prefix \\ nil)
  def get_clauses(%{arity: 0}, _), do: ""
  def get_clauses(%{args: args, name: name}, prefix) do
    get_clauses(args, name, (if prefix, do: prefix <> "."))
  end

  defp get_clauses([env | rest], name, prefix) when env in @env_types do
    get_clauses(rest, name, prefix)
  end
  defp get_clauses(args, name, prefix) do
    [args
    |> Enum.with_index
    |> Enum.map(&get_clause(&1, name, prefix)),
    "\n"]
  end

  defp get_clause({term, index}, function, prefix) when term in ["beam.term", "e.ErlNifTerm"] do
    "  var __#{function}_arg#{index}__ = #{prefix}argv[#{index}];\n"
  end
  defp get_clause({"[]u8", index}, function, prefix) do
    ## NB: we don't deallocate strings because the BEAM returns a pointer to memory space that it owns.
    """
      var __#{function}_arg#{index}__ = beam.get_char_slice(env, #{prefix}argv[#{index}])
        catch return beam.raise_function_clause_error(env);
    """
  end
  defp get_clause({"[]" <> type, index}, function, prefix) do
    """
      var __#{function}_arg#{index}__ = beam.get_slice_of(#{short_name type}, env, #{prefix}argv[#{index}]) catch |err| switch (err) {
        error.OutOfMemory => return beam.raise_enomem(env),
        beam.Error.FunctionClauseError => return beam.raise_function_clause_error(env)
      };
      defer beam.allocator.free(__#{function}_arg#{index}__);
    """
  end
  defp get_clause({type, index}, function, prefix) do
    """
      var __#{function}_arg#{index}__ = beam.get_#{short_name type}(env, #{prefix}argv[#{index}])
        catch return beam.raise_function_clause_error(env);
    """
  end

  def make_clause(type, var, env \\ "env")
  def make_clause("[]u8", var, env) do
    "beam.make_slice(#{env}, #{var})"
  end
  def make_clause("[]" <> type, var, env) do
    "beam.make_#{type}_list(#{env}, #{var}) catch return beam.raise_enomem(env)"
  end
  def make_clause(type, var, env) do
    "beam.make_#{short_name type}(#{env}, #{var})"
  end

  defp short_name("beam.pid"), do: "pid"
  defp short_name("e.ErlNifPid"), do: "pid"
  defp short_name(any), do: any

end
