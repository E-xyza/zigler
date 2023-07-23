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

  @enforce_keys [:export, :concurrency, :type]

  @impl true
  defdelegate fetch(function, key), to: Map

  defstruct @enforce_keys ++ ~w(name raw args return leak_check alias doc spec)a

  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Nif.Threaded
  alias Zig.Nif.Yielding
  alias Zig.Type
  alias Zig.Type.Error
  alias Zig.Type.Function

  @type t :: %__MODULE__{
          name: atom,
          export: boolean,
          concurrency: Synchronous | Threaded | Yielding | DirtyCpu | DirtyIo,
          type: Function.t(),
          raw: nil | :beam | :erl_nif | :c,
          args: [keyword],
          return: keyword,
          leak_check: boolean(),
          alias: nil | atom,
          doc: nil | String.t(),
          spec: Macro.t()
        }

  defmodule Concurrency do
    @moduledoc """
    behaviour module which describes the interface for "plugins" which
    generate concurrency-specific code.
    """

    alias Zig.Nif

    @callback render_elixir(Nif.t()) :: Macro.t()
    @callback render_erlang(Nif.t()) :: term
    @callback render_zig(Nif.t()) :: iodata

    @type concurrency :: :synchronous | :dirty_cpu | :dirty_io
    @type table_entry ::
            {name :: atom, arity :: non_neg_integer, function_pointer :: atom,
             bootstrap :: concurrency}

    @doc """
    returns "table_entry" tuples which are then used to generate the nif table.
    if a nif function needs multiple parts, for example, for concurrency
    management, then multiple entries should be returned.
    """
    @callback table_entries(Nif.t()) :: [table_entry]
    @callback resources(Nif.t()) :: [{:root, atom}]
  end

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
      export: Keyword.fetch!(opts, :export),
      concurrency: Map.get(@concurrency_modules, Keyword.fetch!(opts, :concurrency)),
      type: opts[:type],
      raw: extract_raw(opts[:raw], opts[:type]),
      args: opts[:args],
      return: opts[:return],
      leak_check: opts[:leak_check],
      alias: opts[:alias],
      doc: opts[:doc],
      spec: Keyword.get(opts, :spec, :auto)
    }
  end

  defp extract_raw(raw_opt, %{return: return}) do
    case {raw_opt, return} do
      {nil, _} -> nil
      {{:c, arity}, _} when is_integer(arity) -> :c
      {arity, :term} when is_integer(arity) -> :beam
      {arity, :erl_nif_term} when is_integer(arity) -> :erl_nif
    end
  end

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

        [{:->, _, [params, res]}] ->
          quote do
            @spec unquote(nif.name)(unquote_splicing(params)) :: unquote(res)
          end

        :auto ->
          quote do
            @spec unquote(spec(nif))
          end
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

  def indexed_parameters([:env | rest]) do
    indexed_parameters(rest)
  end

  def indexed_parameters(params_list) do
    Enum.with_index(params_list)
  end

  def indexed_args([:env | rest]) do
    case indexed_args(rest) do
      "" -> "env"
      argstrs -> "env, #{argstrs}"
    end
  end

  def indexed_args(params_list) do
    params_list
    |> Enum.with_index()
    |> Enum.map_join(", ", fn {_, index} -> "arg#{index}" end)
  end

  def maybe_catch(%Error{}) do
    """
    catch |err| {
        return beam.raise_with_error_return(env, err, @errorReturnTrace()).v;
    }
    """
  end

  def maybe_catch(_), do: nil

  def validate_return!(function, file, line) do
    unless Type.return_allowed?(function.return) do
      raise CompileError,
        description: "functions returning #{function.return} are not allowed",
        file: Path.relative_to_cwd(file),
        line: line
    end
  end

  def spec(nif) do
    if nif.raw do
      spec_raw(nif)
    else
      spec_coded(nif)
    end
  end

  defp spec_raw(%{type: type}) do
    params =
      List.duplicate(
        quote do
          term()
        end,
        type.arity
      )

    quote context: Elixir do
      unquote(type.name)(unquote_splicing(params)) :: term()
    end
  end

  defp spec_coded(%{type: type, return: return_opts}) do
    trimmed =
      case type.params do
        [:env | list] -> list
        list -> list
      end

    param_types = Enum.map(trimmed, &Type.spec(&1, :param, []))

    # TODO: check for easy_c
    return =
      if arg = return_opts[:arg] do
        type.params
        |> Enum.at(arg)
        |> Type.spec(:return, return_opts)
      else
        Type.spec(type.return, :return, return_opts)
      end

    quote context: Elixir do
      unquote(type.name)(unquote_splicing(param_types)) :: unquote(return)
    end
  end

  #############################################################################
  ## OPTIONS NORMALIZATION

  # nifs are either atom() or {atom(), keyword()}.  This turns all atom nifs
  # into {atom(), []}, so that downstream they are easier to deal with.

  @doc false
  def default_options,
    do: [
      concurrency: :synchronous,
      args: nil,
      return: [type: :default],
      export: true
    ]

  @doc false
  def normalize_options!(function_name, common_options) when is_atom(function_name) do
    {function_name, common_options}
  end

  def normalize_options!({function_name, opts}, common_options) do
    updated_opts =
      default_options()
      |> Keyword.merge(common_options)
      |> Keyword.merge(Enum.map(opts, &normalize_option!/1))

    {function_name, updated_opts}
  end

  @nif_option_types %{
    concurrency: "one of `:synchronous`, `:dirty_cpu`, `:dirty_io`, `:threaded`, `:yielding`",
    leak_check: "boolean",
    alias: "atom",
    export: "boolean"
  }

  @atom_options %{
    leak_check: {:leak_check, true},
    def: {:export, true},
    defp: {:export, false},
    dirty_cpu: {:concurrency, :dirty_cpu},
    dirty_io: {:concurrency, :dirty_io},
    synchronous: {:concurrency, :synchronous},
    threaded: {:concurrency, :threaded},
    yielding: {:concurrency, :yielding}
  }

  defp normalize_option!(atom) when is_map_key(@atom_options, atom),
    do: Map.fetch!(@atom_options, atom)

  defp normalize_option!(:defp), do: {}

  defp normalize_option!({:leak_check, should_check} = option) when is_boolean(should_check),
    do: option

  defp normalize_option!({:alias, function} = option) when is_atom(function), do: option

  defp normalize_option!({:export, should_export} = option) when is_boolean(should_export),
    do: option

  defp normalize_option!({:raw, integer} = option) when is_integer(integer), do: option

  defp normalize_option!({:raw, {:c, integer}} = option) when is_integer(integer), do: option

  @return_types [:list, :binary, :default]
  @return_option_types %{
    raw: "integer or `{:c, integer}`",
    arg: "integer",
    type: "one of `:list`, `:binary`, `:default`",
    noclean: "boolean",
    length: "`integer` or `{:arg, integer}`"
  }

  @as_is_options ~w(args spec docs)a

  defp normalize_option!({:return, return_opts}) do
    updated_return =
      return_opts
      |> List.wrap()
      |> Enum.map(fn
        integer when is_integer(integer) ->
          {:arg, integer}

        type when type in @return_types ->
          {:type, type}

        :noclean ->
          {:noclean, true}

        option = {:raw, integer} when is_integer(integer) ->
          option

        option = {:raw, {:c, integer}} when is_integer(integer) ->
          option

        option = {:arg, integer} when is_integer(integer) ->
          option

        option = {:type, type} when type in @return_types ->
          option

        option = {:noclean, boolean} when is_boolean(boolean) ->
          option

        option = {:length, integer} when is_integer(integer) ->
          option

        option = {:length, {:arg, integer}} when is_integer(integer) ->
          option

        {ret_opt, _} when is_map_key(@return_option_types, ret_opt) ->
          raise CompileError,
            description:
              "return option `:#{ret_opt}` must be #{Map.fetch!(@return_option_types, ret_opt)}"

        other ->
          raise CompileError, description: "unrecognized return option `#{inspect(other)}`"
      end)
      |> Keyword.put_new(:type, :default)

    {:return, updated_return}
  end

  defp normalize_option!({tag, _} = as_is) when tag in @as_is_options, do: as_is

  defp normalize_option!({opt, value}) when is_map_key(@nif_option_types, opt) do
    raise CompileError,
      description:
        "nif option :#{opt} must be #{Map.fetch!(@nif_option_types, opt)}, got: #{inspect(value)}"
  end

  defp normalize_option!(other) do
    raise CompileError, description: "unrecognized nif option `#{inspect(other)}`"
  end

  # Access behaviour guards
  @impl true
  def get_and_update(_, _, _), do: raise("you should not update a function")

  @impl true
  def pop(_, _), do: raise("you should not pop a function")
end
