defmodule Zig.Nif do
  @moduledoc """
  module encapsulating all of the information required to correctly generate
  a nif function.

  Note that all information obtained from semantic analysis of the function is
  stashed in the `Zig.Nif.Function` module.
  """

  @enforce_keys [:export, :concurrency, :type]

  defstruct @enforce_keys ++
              [
                :opts,
                :raw,
                :file,
                :line,
                :doc
              ]

  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Nif.Threaded
  alias Zig.Nif.Yielding
  alias Zig.Type.Error
  alias Zig.Type.Function
  alias Zig.Resources

  @type t :: %__MODULE__{
          export: boolean,
          concurrency: Synchronous | Threaded | Yielding | DirtyCpu | DirtyIo,
          type: Function.t(),
          opts: keyword(),
          # NB "c" not supported yet
          raw: :zig | :c,
          file: nil | Path.t(),
          line: nil | non_neg_integer(),
          doc: nil | String.t()
        }

  defmodule Concurrency do
    @moduledoc """
    behaviour module which describes the interface for "plugins" which
    generate concurrency-specific code.
    """

    alias Zig.Nif

    @callback render_elixir(Zig.t()) :: Macro.t()
    @callback render_erlang(Zig.t()) :: term
    @callback render_zig(Nif.t()) :: iodata

    @type concurrency :: :synchronous | :dirty_cpu | :dirty_io
    @type table_entry :: {name :: atom, arity :: non_neg_integer, bootstrap :: concurrency}

    @doc """
    returns "table_entry" tuples which are then used to generate the nif table.
    if a nif function needs multiple parts, for example, for concurrency
    management, then multiple entries should be returned.
    """
    @callback functions(Nif.t()) :: [table_entry]
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
  def new(opts) do
    %__MODULE__{
      export: Keyword.fetch!(opts, :export),
      concurrency: Map.get(@concurrency_modules, Keyword.fetch!(opts, :concurrency)),

    }
  end

  # function retrieval and manipulation
  defp find_function(sema_list, name) do
    Enum.find(sema_list, &(&1.name == name)) || raise "unreachable"
  end

  defp adopt_options(function, :auto), do: function
  defp adopt_options(function, opts), do: %{function | opts: opts}

  def render_elixir(nif = %{concurrency: concurrency}, opts \\ []) do
    typespec =
      if Keyword.get(opts, :typespec?, true) do
        quote do
          @spec unquote(Function.spec(function))
        end
      end

    functions = concurrency.render_elixir(nif)

    quote context: Elixir do
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

  @spec needs_marshal?(t) :: boolean
  # TODO: move this query into the concurrency module.
  def needs_marshal?(nif) do
    !!nif.param_marshalling_macros or !!nif.return_marshalling_macro or !!nif.param_error_macros or
      match?(%Error{}, nif.function.return)
  end

  require EEx

  nif = Path.join(__DIR__, "templates/nif.zig.eex")
  EEx.function_from_file(:def, :nif_file, nif, [:assigns])

  def render_zig(nifs, resources, module) do
    nif_file(binding())
  end

  def render_zig(nif = %__MODULE__{}) do
    nif.concurrency.render_zig(nif)
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
        return e.enif_raise_exception(env, beam.make(env, .{ .@"error", err, @errorReturnTrace()}, .{}).v);
    }
    """
  end

  def maybe_catch(_), do: nil

  # internal helpers
  defp table_entries(nifs) when is_list(nifs) do
    Enum.map_join(nifs, ", ", &table_entries/1)
  end

  defp table_entries(nif) do
    nif.concurrency.table_entries(nif)
  end

  @index_of %{major: 0, minor: 1}

  defp nif_version(at) do
    :nif_version
    |> :erlang.system_info()
    |> List.to_string()
    |> String.split(".")
    |> Enum.at(@index_of[at])
  end

  def validate_return!(function) do
    unless Type.return_allowed?(function.return) do
      raise CompileError,
        description: "functions returning #{function.return} are not allowed",
        file: function.file,
        line: function.line
    end
  end

  #############################################################################
  ## OPTIONS NORMALIZATION

  # nifs are either atom() or {atom(), keyword()}.  This turns all atom nifs
  # into {atom(), []}, so that downstream they are easier to deal with.

  @doc false
  def normalize_options!(function_name) when is_atom(function_name) do
    {function_name, []}
  end

  def normalize_options!({function_name, opts}) do
    {function_name, Enum.map(opts, &normalize_option/1)}
  end

  # TODO: figure this out later
  defp normalize_option!(option), do: option
end
