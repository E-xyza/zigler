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

  @enforce_keys ~w[name export module concurrency file module_code_path zig_code_path spec]a

  defstruct @enforce_keys ++ ~w[line signature params return leak_check alias doc raw impl]a

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
           module: module,
           concurrency: Concurrency.t(),
           line: integer,
           file: Path.t(),
           module_code_path: Path.t(),
           zig_code_path: Path.t(),
           spec: boolean(),
           signature: Function.t(),
           params: integer,
           return: Return.t(),
           leak_check: boolean(),
           alias: nil | atom,
           doc: nil | String.t(),
           raw: :term | :erl_nif_term,
           impl: boolean | module
         }

  @typep specified :: %__MODULE__{
           name: atom,
           export: boolean,
           concurrency: Concurrency.t(),
           module: module,
           line: integer,
           file: Path.t(),
           module_code_path: Path.t(),
           zig_code_path: Path.t(),
           spec: boolean(),
           signature: Function.t(),
           params: %{optional(integer) => Parameter.t()},
           return: Return.t(),
           leak_check: boolean(),
           alias: nil | atom,
           doc: nil | String.t(),
           raw: nil,
           impl: boolean | module
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
  @concurrency_opts Map.keys(@concurrency_modules)
  @defaults %{
    :cleanup => true,
    :leak_check => false
  }
  @defaultable_opts Map.keys(@defaults)

  @doc """
  based on nif options for this function keyword at (opts :: nifs :: function_name)
  """
  def new(name, module, opts!) do
    opts! = adjust(opts!)

    %__MODULE__{
      name: name,
      file: module.file,
      module: module.module,
      module_code_path: module.module_code_path,
      zig_code_path: module.zig_code_path,
      export: Keyword.get(opts!, :export, true),
      concurrency: Map.get(@concurrency_modules, opts![:concurrency], Synchronous),
      spec: Keyword.get(opts!, :spec, true),
      leak_check: Keyword.get(opts!, :leak_check, @defaults[:leak_check]),
      params: opts![:params],
      alias: opts![:alias],
      impl: opts![:impl]
    }
  end

  def adjust(opts) do
    Enum.map(opts, fn
      defaultable when defaultable in @defaultable_opts -> {defaultable, true}
      :nocleanup -> {:cleanup, false}
      concurrency when concurrency in @concurrency_opts -> {:concurrency, concurrency}
      {atom, _} = kv when is_atom(atom) -> kv
    end)
  end

  def arities(%{raw: nil, signature: %{arity: arity}}), do: [arity]
  def arities(%{raw: t, params: arities}) when not is_nil(t), do: arities

  def render_elixir(%{concurrency: concurrency} = nif) do
    doc =
      if nif_doc = nif.doc do
        quote do
          @doc unquote(nif_doc)
        end
      end

    functions = concurrency.render_elixir(nif)
    specs = if nif.spec, do: render_elixir_spec(nif)

    impl =
      nif.impl
      |> List.wrap()
      |> Enum.map(
        &quote do
          @impl unquote(&1)
        end
      )

    quote context: Elixir do
      unquote(specs)
      unquote(doc)
      unquote(impl)
      unquote(functions)
    end
  end

  def render_elixir_spec(%{raw: t, params: arities} = nif) when not is_nil(t) do
    Enum.map(arities, fn arity ->
      param_spec =
        case arity do
          0 -> []
          arity -> Enum.map(0..(arity - 1), fn _ -> {:term, [], []} end)
        end

      quote do
        @spec unquote(nif.name)(unquote_splicing(param_spec)) :: term
      end
    end)
  end

  def render_elixir_spec(nif) do
    param_spec =
      nif.params
      |> Enum.sort()
      |> Enum.map(fn {_, p} -> Type.render_elixir_spec(p.type, p) end)

    return_spec =
      case nif.return do
        %{spec: nil} ->
          Type.render_elixir_spec(nif.return.type, nif.return)

        %{type: term, spec: spec} when term in ~w[term erl_nif_term]a ->
          spec

        _ ->
          raise CompileError,
            description:
              "you may only specify return spec when the return type is `term` or `e.ErlNifTerm`"
      end

    quote context: Elixir do
      @spec unquote(nif.name)(unquote_splicing(param_spec)) :: unquote(return_spec)
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
    nif
    |> nif.concurrency.table_entries()
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

  def resources(nif), do: nif.concurrency.resources(nif)

  @spec set_file_line(t(), module, Parser.t()) :: t()
  def set_file_line(nif, manifest_module, parsed) do
    expected_name = if name = nif.alias, do: name, else: nif.name

    raw_line =
      Enum.find_value(parsed.code, fn
        %{name: name, location: {line, _}} -> if name == expected_name, do: line
      end)

    {file, line} = manifest_module.__resolve(%{file_name: nif.zig_code_path, line: raw_line})

    %{nif | file: file, line: line}
  end

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

  def style(nif) do
    if nif.export, do: :def, else: :defp
  end

  def binding_error(name, arity) do
    "nif for function #{name}/#{arity} not bound"
  end

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")
end
