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

  @enforce_keys ~w[name export concurrency file]a

  defstruct @enforce_keys ++ ~w[line signature params return leak_check alias doc spec raw]a

  alias Zig.Nif.Concurrency
  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Nif.Threaded
  alias Zig.Nif.Yielding
  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  alias Zig.Type.Error
  alias Zig.Type.Function

  @typep raw :: %__MODULE__{
          name: atom,
          export: boolean,
          concurrency: Concurrency.t(),
          line: integer,
          file: Path.t(),
          signature: Function.t(),
          params: integer,
          return: Return.t(),
          leak_check: boolean(),
          alias: nil | atom,
          doc: nil | String.t(),
          spec: Macro.t(),
          raw: :term | :erl_nif_term
        }

  @typep specified :: %__MODULE__{
          name: atom,
          export: boolean,
          concurrency: Concurrency.t(),
          line: integer,
          file: Path.t(),
          signature: Function.t(),
          params: %{optional(integer) => Parameter.t()},
          return: Return.t(),
          leak_check: boolean(),
          alias: nil | atom,
          doc: nil | String.t(),
          spec: Macro.t(),
          raw: nil
        }

  @type t :: raw | specified

  @type defaultable_opts ::
          {:cleanup, boolean}
          | {:leak_check, boolean}

  @type individual_opts ::
          {:ignore, boolean}
          | {:export, boolean}
          | {:concurrency, Concurrency.t()}
          | {:args, %{optional(integer) => Parameter.opts()}}
          | {:return, Return.opts()}
          | {:alias, atom()}
          | {:doc, String.t()}
          | {:spec, Macro.t()}

  @type opts() :: [defaultable_opts | individual_opts]

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
  def new(name, file, opts) do
    %__MODULE__{
      name: name,
      file: file,
      export: Keyword.get(opts, :export, true),
      concurrency: Map.get(@concurrency_modules, opts[:concurrency], Synchronous)
    }
  end

  def arity(%{raw: nil, signature: %{arity: arity}}), do: arity
  def arity(%{raw: t, params: arity}) when not is_nil(t), do: arity

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
            @spec unquote(render_elixir_spec(nif))
          end
      end

    functions = concurrency.render_elixir(nif)

    quote context: Elixir do
      unquote(doc)
      unquote(typespec)
      unquote(functions)
    end
  end

  def render_elixir_spec(%{raw: t, params: arity} = nif) when not is_nil(t) do
    param_spec = case arity do
      0 -> []
      arity -> Enum.map(0..arity - 1, fn _ -> {:term, [], []} end)
    end

    return_spec = Type.render_elixir_spec(nif.return.type, :return, nif.return)

    quote do
      unquote(nif.name)(unquote_splicing(param_spec)) :: unquote(return_spec)
    end
  end

  def render_elixir_spec(nif) do
    param_spec =
      nif.params
      |> Enum.sort()
      |> Enum.map(fn {_, p} -> Type.render_elixir_spec(p.type, :param, p) end)

    return_spec = Type.render_elixir_spec(nif.return.type, :return, nif.return)

    quote context: Elixir do
      unquote(nif.name)(unquote_splicing(param_spec)) :: unquote(return_spec)
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
      {function, fptr, concurrency} ->
        flags = Map.fetch!(@flags, concurrency)
        ~s(.{.name="#{function}", .arity=#{arity(nif)}, .fptr=#{fptr}, .flags=#{flags}})
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

  # COMMON TOOLS
  # generates AST for parameters.  
  @spec elixir_parameters(arity, used :: boolean) :: [Macro.t()]
  def elixir_parameters(0, _), do: []
  def elixir_parameters(arity, true), do: Enum.map(1..arity, &{:"arg#{&1}", [], Elixir})
  def elixir_parameters(arity, false), do: Enum.map(1..arity, &{:"_arg#{&1}", [], Elixir})

  @spec erlang_parameters(arity, used :: boolean) :: [{:var, atom()}]
  def erlang_parameters(0, _), do: []
  def erlang_parameters(arity, true), do: Enum.map(1..arity, &{:var, :"X#{&1}"})
  def erlang_parameters(arity, false), do: Enum.map(1..arity, &{:var, :"_X#{&1}"})

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")
end
