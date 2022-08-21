defmodule Zig.Nif do
  defstruct [:type, :concurrency, :name, :function]

  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Nif.Threaded
  alias Zig.Nif.Yielding
  alias Zig.Type
  alias Zig.Type.Function

  @type t :: %__MODULE__{
          type: :def | :defp,
          concurrency: Synchronous | Threaded | Yielding | DirtyCpu | DirtyIo,
          function: Function.t()
        }

  defp normalize_all(:all, functions) do
    Enum.map(functions, &{&1.name, []})
  end

  defp normalize_all(list, _) when is_list(list), do: list

  def from_sema(sema_list, nif_opts) do
    nif_opts
    |> normalize_all(sema_list)
    |> Enum.map(fn
      {name, opts} ->
        %__MODULE__{
          type: Keyword.get(opts, :defp) || :def,
          concurrency: Synchronous,
          function: find_function(name, sema_list)
        }
    end)
  end

  defp find_function(name, sema_list) do
    Enum.find(sema_list, &(&1.name == name)) || raise "unreachable"
  end

  def render_elixir(nif) do
    # for now.
    typespec = nil

    marshalling = [
      Type.marshal_zig(nif.function.return)
      | Enum.flat_map(nif.function.params, &List.wrap(Type.marshal_elixir(&1)))
    ]

    needs_marshalling? = Enum.any?(marshalling)

    function = nif.concurrency.render_elixir(nif)

    if needs_marshalling? do
      quote context: Elixir do
        unquote(typespec)
        unquote(marshalled(marshalling, nif.function.name))
        unquote(function)
      end
    else
      quote context: Elixir do
        unquote(typespec)
        unquote(function)
      end
    end
  end

  defp marshalled([rfunc | pfuncs], name) do
    return = {:return, [], Elixir}
    return_clause = if rfunc, do: rfunc.(return), else: return

    {args, args_clauses} =
      pfuncs
      |> Enum.with_index()
      |> Enum.map(fn
        {maybe_func, index} ->
          arg = {:"arg#{index}", [], Elixir}
          clause = if maybe_func, do: maybe_func.(arg)
          {arg, clause}
      end)
      |> Enum.unzip()

    quote do
      defp unquote(:"marshalling_#{name}")(unquote_splicing(args)) do
        unquote_splicing(args_clauses)
        unquote(return) = unquote(name)(unquote_splicing(args))
        unquote(return_clause)
      end
    end
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

  def indexed_parameters(params_list) do
    Enum.with_index(params_list)
  end

  def indexed_args(params_list) do
    params_list
    |> Enum.with_index()
    |> Enum.map_join(", ", fn {_, index} -> "arg#{index}" end)
  end

  # internal helpers
  defp table_entries(nifs) do
    Enum.map_join(nifs, ", ", &table_entry/1)
  end

  defp table_entry(nif) do
    nif.concurrency.table_entry(nif)
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
