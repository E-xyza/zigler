defmodule Zig.Nif do
  @moduledoc false

  # module encapsulating all of the information required to correctly generate
  # a nif function.
  #
  # Note that all information obtained from semantic analysis of the function is
  # stashed in the `Zig.Nif.Function` module.

  # This module gets an access behaviour so that it can be easily used in EEx files.
  @behaviour Access

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

  @enforce_keys ~w[name module file line module_code_path zig_code_path cleanup]a

  defstruct @enforce_keys ++
              [
                :allocator,
                :impl,
                :alias,
                # next three are determined internally as a part of parsing or sema.
                :signature,
                :raw,
                :doc,
                # user-specified options with defaults
                # note that `cleanup` default is set programatically since `return` depends on it.
                params: %{},
                return: [],
                export: true,
                concurrency: Synchronous,
                spec: true,
                leak_check: false
              ]

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
           allocator: nil | atom,
           params: integer | %{optional(integer) => Parameter.t()},
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
          {:export, boolean}
          | {:concurrency, Concurrency.t()}
          | {:params, %{optional(integer) => Parameter.opts()}}
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
  def new(name, opts, caller) do
    opts
    |> normalize(name, caller)
    |> dbg(limit: 25)
    |> then(&struct!(__MODULE__, &1))
  end

  def normalize(opts, name, module) do
    opts
    |> Enum.map(fn
      defaultable when defaultable in @defaultable_opts ->
        {defaultable, true}

      :noclean ->
        {:cleanup, false}

      :nospec ->
        {:spec, false}

      concurrency when concurrency in @concurrency_opts ->
        {:concurrency, Map.fetch!(@concurrency_modules, concurrency)}

      {:concurrency, concurrency} when concurrency in @concurrency_opts ->
        {:concurrency, Map.fetch!(@concurrency_modules, concurrency)}

      {:concurrency, concurrency} ->
        text = Enum.map_join(@concurrency_opts, ", ", &"`#{inspect(&1)}`")

        raise CompileError,
          description:
            "nif option `concurrency` must be one of #{text}, got: `#{inspect(concurrency)}`",
          file: module.file,
          line: module.line

      {:params, params} when is_integer(params) and params >= 0 ->
        {:params, params}

      {:params, params} when is_map(params) ->
        {:params,
         Map.new(params, fn
           {index, opts} when is_integer(index) and index >= 0 ->
             {index, Parameter.new(nil, opts, module)}

           {other, _} ->
             raise CompileError,
               description:
                 "nif option `params` map keys must be non-negative integers, got: `#{inspect(other)}`",
               file: module.file,
               line: module.line
         end)}

      {:params, error} ->
        raise CompileError,
          description:
            "nif option `params` must be a non-negative integer or a params map, got: `#{inspect(error)}`",
          file: module.file,
          line: module.line

      {:alias, ^name} ->
        raise CompileError,
          description: "nif option `alias` cannot be the same as the nif name",
          file: module.file,
          line: module.line

      {:alias, a} when is_atom(a) ->
        {:alias, a}

      {:alias, a} ->
        raise CompileError,
          description: "nif option `alias` must be an atom, got: `#{inspect(a)}`",
          file: module.file,
          line: module.line

      {:doc, doc} when is_binary(doc) ->
        {doc, doc}

      {:export, v} when not is_boolean(v) ->
        raise CompileError,
          description: "nif option `export` must be a boolean, got: `#{inspect(v)}`",
          file: module.file,
          line: module.line

      {:spec, v} when not is_boolean(v) ->
        raise CompileError,
          description: "nif option `spec` must be a boolean, got: `#{inspect(v)}`",
          file: module.file,
          line: module.line

      {:impl, v} when is_atom(v) ->
        {:impl, v}

      {:impl, v} ->
        raise CompileError,
          description: "nif option `impl` must be a module or `true`, got: `#{inspect(v)}`",
          file: module.file,
          line: module.line

      :leak_check ->
        {:leak_check, true}

      {:leak_check, v} when is_boolean(v) ->
        {:leak_check, v}

      {:leak_check, v} ->
        raise CompileError,
          description: "nif option `leak_check` must be a boolean, got: `#{inspect(v)}`",
          file: module.file,
          line: module.line

      {:cleanup, v} when is_boolean(v) ->
        {:cleanup, v}

      {:cleanup, v} ->
        raise CompileError,
          description: "nif option `cleanup` must be a boolean, got: `#{inspect(v)}`",
          file: module.file,
          line: module.line

      {:allocator, v} when is_atom(v) ->
        {:allocator, v}

      {:allocator, v} ->
        raise CompileError,
          description: "nif option `allocator` must be an atom, got: `#{inspect(v)}`",
          file: module.file,
          line: module.line

      {atom, _} = kv when is_atom(atom) ->
        kv
    end)
    |> Keyword.put_new(:cleanup, true)
    |> Keyword.put(:name, name)
    |> normalize_return(module)
  end

  defp normalize_return(opts, module) do
    cleanup = Keyword.fetch!(opts, :cleanup)

    Keyword.update(
      opts,
      :return,
      %Return{cleanup: cleanup},
      &Return.normalize_options(&1, cleanup, module)
    )
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
        if (beam.thread_not_running(err)) return 0;
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
      end) || raise "raw line information not found for #{expected_name}"

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
