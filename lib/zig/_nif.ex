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
  alias Zig.Options
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
                :arity,
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
           raw: nil | :term | :erl_nif_term,
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

  @doc """
  based on nif options for this function keyword at (opts :: nifs :: function_name)
  """
  def new({name, opts}, context) do
    # all options which can take atoms must be normalized first.
    {opts, context} =
      opts
      |> Options.normalize_boolean(:cleanup, context, noclean: false)
      |> pull_clean(context)

    opts
    |> Options.normalize_boolean(:leak_check, context, leak_check: true)
    |> Options.normalize_boolean(:spec, context, nospec: false)
    |> Options.normalize(:concurrency, &normalize_concurrency/2, context)
    |> Options.scrub_non_keyword(context)
    |> Keyword.put(:name, name)
    |> Options.normalize_as_struct(:params, {:int_map, Parameter}, context)
    |> Options.normalize_as_struct(:return, Return, context)
    |> Options.normalize_kw(:arity, &normalize_arity/2, context)
    |> Options.validate(:impl, {:atom, "a module or `true`"}, context)
    |> Options.validate(:alias, &validate_alias(&1, name), context)
    |> Options.validate(:export, :boolean, context)
    |> Options.validate(:allocator, :atom, context)
    |> then(&struct!(__MODULE__, &1))
  rescue
    e in KeyError ->
      Options.raise_with("was supplied the invalid option `#{e.key}`", context)
  end

  defp pull_clean(opts, context), do: pull_clean(opts, opts, context)

  defp pull_clean([{:cleanup, value} | _], opts, context), do: {opts, %{context | cleanup: value}}
  defp pull_clean([_ | rest], opts, context), do: pull_clean(rest, opts, context)
  defp pull_clean([], opts, context), do: {[{:cleanup, context.cleanup} | opts], context}

  @concurrency_map %{
    synchronous: Synchronous,
    threaded: Threaded,
    yielding: Yielding,
    dirty_cpu: DirtyCpu,
    dirty_io: DirtyIo
  }

  defp normalize_concurrency({atom}, _context), do: Map.fetch(@concurrency_map, atom)

  defp normalize_concurrency(value, context) do
    case Map.fetch(@concurrency_map, value) do
      {:ok, module} ->
        module

      :error ->
        valid_options =
          @concurrency_map
          |> Map.keys()
          |> Options.list_of()

        Options.raise_with("must be one of #{valid_options}", value, context)
    end
  end

  defp normalize_arity(arity, context) do
    arity
    |> List.wrap()
    |> Enum.flat_map(fn
      n when is_integer(n) and n >= 0 ->
        [n]

      a.._//1 = range when a >= 0 ->
        Enum.to_list(range)

      other ->
        Options.raise_with(
          "must be a non-negative integer, range or a list of those",
          other,
          context
        )
    end)
  end

  defp validate_alias(name, name), do: {:error, "may not be the same as the nif name `#{name}`"}

  defp validate_alias(alias_name, _name) when is_atom(alias_name), do: :ok

  defp validate_alias(other, _name), do: {:error, "must be an atom", other}

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

  def render_elixir_spec(%{raw: t, arity: arities} = nif) when not is_nil(t) do
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

  def merge(sema_nif, spec_nif) do
    # these are the fields we are going to merge
    merge_fields = ~w[name cleanup allocator impl alias export concurrency spec leak_check]a

    # params and return needs to be deep-merged.
    merge_fields
    |> Enum.reduce(sema_nif, fn field, so_far ->
      Map.replace!(so_far, field, Map.fetch!(spec_nif, field))
    end)
    |> merge_arity(spec_nif)
    |> Map.update!(:return, &Return.merge(&1, spec_nif.return))
    |> Map.update!(:params, &deepmerge_params(&1, spec_nif.params))
  end

  defp merge_arity(%{raw: nil} = sema_nif, %{arity: nil}),
    do: %{sema_nif | arity: [sema_nif.signature.arity]}

  defp merge_arity(%{raw: nil, arity: arity}, spec_nif) do
    raise CompileError,
      description:
        "the non-raw function #{inspect(spec_nif.module)}.#{spec_nif.name}/#{arity} may not have an arity specified in the nif parameters",
      file: spec_nif.file,
      line: spec_nif.line
  end

  defp merge_arity(sema_nif, %{arity: arity}) when is_list(arity), do: %{sema_nif | arity: arity}

  defp merge_arity(_, spec_nif) do
    raise CompileError,
      description:
        "the raw function #{inspect(spec_nif.module)}.#{spec_nif.name}/? must have arities specified in zigler parameters",
      file: spec_nif.file,
      line: spec_nif.line
  end

  defp deepmerge_params(sema_params, spec_params) do
    Enum.reduce(spec_params, sema_params, fn
      {index, parameter}, so_far ->
        Map.update!(so_far, index, &Parameter.merge(&1, parameter))
    end)
  end

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")
end
