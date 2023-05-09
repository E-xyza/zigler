defmodule Zig.Nif.Threaded do
  @behaviour Zig.Nif.Concurrency

  alias Zig.Nif.Basic
  require EEx

  @impl true
  def render_elixir(nif = %{type: type}) do
    def_or_defp = if nif.export, do: :def, else: :defp
    entrypoint = entrypoint(nif)

    {empty_params, used_params} =
      case type.arity do
        0 ->
          {[], []}

        n ->
          1..n
          |> Enum.map(&{{:"_arg#{&1}", [], Elixir}, {:"arg#{&1}", [], Elixir}})
          |> Enum.unzip()
      end

    quote context: Elixir do
      unquote(def_or_defp)(unquote(type.name)(unquote_splicing(used_params))) do
        ref = unquote(entrypoint)(unquote_splicing(used_params))

        receive do
          {:ok, ^ref, result} ->
            result

          {:error, ^ref, error} ->
            :erlang.raise(error)
        end
      end

      defp unquote(entrypoint)(unquote_splicing(empty_params)) do
        :erlang.nif_error(unquote("foobar"))
      end
    end
  end

  @impl true
  defdelegate render_erlang(nif), to: Basic

  @impl true
  threaded = Path.join(__DIR__, "../templates/threaded.zig.eex")
  EEx.function_from_file(:defp, :render_zig, threaded, [:assigns])

  @impl true
  def table_entries(nif) do
    [{entrypoint(nif), nif.type.arity, :synchronous}]
  end

  defp entrypoint(nif) do
    :"thread-bootstrap-#{nif.type.name}"
  end
end
