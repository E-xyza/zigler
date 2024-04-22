defmodule Zig.Nif do
  @moduledoc """
  module encapsulating all of the information required to correctly generate
  a nif function.

  Note that all information obtained from semantic analysis of the function is
  stashed in the `Zig.Nif.Function` module.
  """

  # nif gets an access behaviour so that it can be easily used in EEx
  # files.
  @behaviour Access

  @enforce_keys ~w[name export concurrency]a

  defstruct @enforce_keys ++ ~w[signature params return leak_check alias doc spec]a

  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Nif.Threaded
  alias Zig.Nif.Yielding
  alias Zig.Type.Error
  alias Zig.Type.Function

  @type t :: %__MODULE__{
          name: atom,
          export: boolean,
          concurrency: Synchronous | Threaded | Yielding | DirtyCpu | DirtyIo,
          signature: Function.t(),
          # TODO: make `term` more specific.
          params: %{optional(integer) => term},
          return: keyword,
          leak_check: boolean(),
          alias: nil | atom,
          doc: nil | String.t(),
          spec: Macro.t()
        }

  @impl true
  defdelegate fetch(function, key), to: Map

  @concurrency_modules %{
    :synchronous => Synchronous,
    :threaded => Threaded,
    :yielding => Yielding,
    :dirty_cpu => DirtyCpu,
    :dirty_io => DirtyIo
  }

  @doc """
  based on nif options for this function keyword at (opts :: nifs :: function_name)
  """
  def new(name, opts) do
    %__MODULE__{
      name: name,
      export: Keyword.get(opts, :export, true),
      concurrency: Map.get(@concurrency_modules, opts[:concurrency], Synchronous)
    }

    #  raw: extract_raw(opts[:raw], opts[:type]),
    #  args: opts[:args],
    #  return: opts[:return],
    #  leak_check: opts[:leak_check],
    #  alias: opts[:alias],
    #  doc: opts[:doc],
    #  spec: Keyword.get(opts, :spec, :auto)
    # }
  end

  # defp extract_raw(raw_opt, %{return: return}) do
  #  case {raw_opt, return} do
  #    {nil, _} -> nil
  #    {{:c, arity}, _} when is_integer(arity) -> :c
  #    {arity, :term} when is_integer(arity) -> :beam
  #    {arity, :erl_nif_term} when is_integer(arity) -> :erl_nif
  #  end
  # end

  def render_elixir(%{concurrency: concurrency} = nif) do
    doc =
      if nif_doc = nif.doc do
        quote do
          @doc unquote(nif_doc)
        end
      end

    typespec =
      case nif.spec do
        false ->
          quote do
          end

        _ ->
          quote do
          end

          # quote do
          #  @spec unquote(Function.render_elixir_spec(nif.spec, nif.name))
          # end
      end

    functions = concurrency.render_elixir(nif)

    quote context: Elixir do
      unquote(doc)
      unquote(typespec)
      unquote(functions)
    end
  end

  def render_erlang(nif, _opts \\ []) do
    # TODO: typespec in erlang.

    function =
      nif
      |> nif.concurrency.render_erlang
      |> List.wrap()

    function
  end

  require EEx

  def render_zig(%__MODULE__{} = nif) do
    nif.concurrency.render_zig(nif)
  end

  @flags %{
    synchronous: "0",
    dirty_cpu: "e.ERL_NIF_DIRTY_JOB_CPU_BOUND",
    dirty_io: "e.ERL_NIF_DIRTY_JOB_IO_BOUND"
  }

  def table_entries(nif) do
    nif.concurrency.table_entries(nif)
    |> Enum.map(fn
      {function, arity, fptr, concurrency} ->
        flags = Map.fetch!(@flags, concurrency)
        ~s(.{.name="#{function}", .arity=#{arity}, .fptr=#{fptr}, .flags=#{flags}})
    end)
  end

  def maybe_catch(%Error{}) do
    """
    catch |err| {
        return beam.raise_with_error_return(err, @errorReturnTrace(), .{}).v;
    }
    """
  end

  def maybe_catch(_), do: nil

  # def validate_return!(function, file, line) do
  #  unless Type.return_allowed?(function.return) do
  #    raise CompileError,
  #      description: "functions returning #{function.return} are not allowed",
  #      file: Path.relative_to_cwd(file),
  #      line: line
  #  end
  # end

  def indexed_parameters(_), do: raise("unimplemented")

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")
end
