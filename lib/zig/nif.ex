defmodule Zig.Nif do
  # TODO: distinguish Nif from Module.
  @enforce_keys [:type, :concurrency, :function]

  defstruct @enforce_keys ++
              [
                :entrypoint,
                :param_marshalling_macros,
                :return_marshalling_macro,
                :param_error_macros
              ]

  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Marshaller
  alias Zig.Nif.Synchronous
  alias Zig.Nif.Threaded
  alias Zig.Nif.Yielding
  alias Zig.Type.Function

  @type t :: %__MODULE__{
          type: :def | :defp,
          concurrency: Synchronous | Threaded | Yielding | DirtyCpu | DirtyIo,
          function: Function.t(),
          # calculated details.
          entrypoint: atom,
          param_marshalling_macros: nil | [nil | (Macro.t() -> Macro.t())],
          return_marshalling_macro: nil | (Macro.t() -> Macro.t()),
          param_error_macros: nil | [nil | (Macro.t() -> Macro.t())]
        }

  defmodule Concurrency do
    @callback render_elixir(Zig.Nif.t()) :: Macro.t()
    @callback render_zig(Zig.Nif.t()) :: iodata
    @callback set_entrypoint(Zig.Nif.t()) :: Zig.Nif.t()
  end

  defp normalize_all(:all, functions) do
    Enum.map(functions, &{&1.name, []})
  end

  defp normalize_all(list, _) when is_list(list), do: list

  @doc """
  obtains a list of Nif structs from the semantically analyzed content and
  the nif options that are a part of
  """
  # TODO: unit test this function directly.
  def from_sema(sema_list, nif_opts) do
    nif_opts
    |> normalize_all(sema_list)
    |> Enum.map(fn
      {name, opts} ->
        # "calculated" details.
        function = find_function(name, sema_list)
        concurrency = Synchronous

        concurrency.set_entrypoint(%__MODULE__{
          type: Keyword.get(opts, :defp) || :def,
          concurrency: concurrency,
          function: function,
          param_marshalling_macros: Function.param_marshalling_macros(function),
          return_marshalling_macro: Function.return_marshalling_macro(function),
          param_error_macros: Function.param_error_macros(function)
        })
    end)
  end

  defp find_function(name, sema_list) do
    Enum.find(sema_list, &(&1.name == name)) || raise "unreachable"
  end

  # for now, don't implement.
  def typespec(_), do: nil

  def render_elixir(nif, opts \\ []) do
    typespec = List.wrap(if Keyword.get(opts, :typespec?, true), do: typespec(nif))

    marshalling = List.wrap(Marshaller.render(nif))

    function =
      nif
      |> nif.concurrency.render_elixir
      |> List.wrap()

    quote context: Elixir do
      (unquote_splicing(Enum.flat_map([typespec, marshalling, function], & &1)))
    end
  end

  @spec needs_marshal?(t) :: boolean
  def needs_marshal?(nif) do
    !!nif.param_marshalling_macros or !!nif.return_marshalling_macro or !!nif.param_error_macros
  end

  require EEx

  nif = Path.join(__DIR__, "templates/nif.zig.eex")
  EEx.function_from_file(:def, :nif_file, nif, [:assigns])

  def render_zig(all_nifs, module) do
    nif_file(%{nifs: all_nifs, module: module})
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
end
