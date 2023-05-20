defmodule Zig.Nif.Threaded do
  @behaviour Zig.Nif.Concurrency

  alias Zig.Nif
  alias Zig.Type
  require EEx

  @impl true
  def render_elixir(nif = %{type: %{name: name, arity: arity}}) do
    def_or_defp = if nif.export, do: :def, else: :defp

    {empty_params, used_params} =
      case arity do
        0 ->
          {[], []}

        n ->
          1..n
          |> Enum.map(&{{:"_arg#{&1}", [], Elixir}, {:"arg#{&1}", [], Elixir}})
          |> Enum.unzip()
      end

    quote context: Elixir do
      unquote(def_or_defp)(unquote(name)(unquote_splicing(used_params))) do
        ref = unquote(entrypoint(nif, :launch))(unquote_splicing(used_params))

        receive do
          {:done, ^ref} ->
            unquote(entrypoint(nif, :join))(ref)
          {:error, ^ref} ->
            raise unquote("thread for function #{name} failed during launch")
        end
      end

      defp unquote(entrypoint(nif, :launch))(unquote_splicing(empty_params)) do
        :erlang.nif_error(unquote("nif for function #{name}/#{arity} not bound"))
      end

      defp unquote(entrypoint(nif, :join))(_ref) do
        :erlang.nif_error(unquote("nif for function #{name}/#{arity} not bound"))
      end
    end
  end

  @impl true
  defdelegate render_erlang(nif), to: Basic

  @impl true
  threaded = Path.join(__DIR__, "../templates/threaded.zig.eex")
  EEx.function_from_file(:def, :render_zig, threaded, [:assigns])

  @impl true
  def table_entries(nif) do
    launch = entrypoint(nif, :launch)
    join = entrypoint(nif, :join)

    [
      {launch, nif.type.arity, :"@\"#{launch}\"", :synchronous},
      {join, 1, :"@\"#{join}\"", :synchronous}
    ]
  end

  defp entrypoint(nif, mode) do
    :"#{nif.type.name}-#{mode}"
  end

  @impl true
  def resources(nif), do: [{:root, :"ThreadResource_#{nif.type.name}"}]
end
