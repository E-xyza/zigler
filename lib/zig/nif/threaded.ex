defmodule Zig.Nif.Threaded do
  @behaviour Zig.Nif.Concurrency

  alias Zig.ErrorProng
  alias Zig.Nif
  alias Zig.Type

  require EEx

  import Zig.QuoteErl

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

    error_prongs =
      nif
      |> error_prongs()
      |> Enum.flat_map(&apply(ErrorProng, &1, [:elixir]))

    quote context: Elixir do
      unquote(def_or_defp)(unquote(name)(unquote_splicing(used_params))) do
        ref = unquote(entrypoint(nif, :launch))(unquote_splicing(used_params))

        receive do
          {:done, ^ref} ->
            unquote(entrypoint(nif, :join))(ref)

          {:error, ^ref} ->
            raise unquote("thread for function #{name} failed during launch")
        end
      catch
        unquote(error_prongs)
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
  def render_erlang(nif = %{type: %{name: name, arity: arity}}) do
    {unused_vars, used_vars} =
      case arity do
        0 ->
          {[], []}

        n ->
          1..n
          |> Enum.map(&{{:var, :"_X#{&1}"}, {:var, :"X#{&1}"}})
          |> Enum.unzip()
      end

    quote_erl(
      """
      unquote(name)(unquote(...vars)) ->
        Ref = unquote(launch_name)(unquote(...vars)),
        receive
          {done, Ref} ->
            unquote(join_name)(Ref);
          {error, Ref} ->
            erlang:error(unquote(launch_fail_text))
        end.

      unquote(launch_name)(unquote(...unused_vars)) ->
        erlang:nif_error(unquote(error_text)).

      unquote(join_name)(_Ref) ->
        erlang:nif_error(unquote(error_text)).
      """,
      name: name,
      launch_name: entrypoint(nif, :launch),
      join_name: entrypoint(nif, :join),
      vars: used_vars,
      unused_vars: unused_vars,
      launch_fail_text: 'thread for function #{name} failed during launch',
      error_text: 'nif for function #{name}/#{arity} not bound'
    )
  end

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

  defp error_prongs(nif) do
    nif.type.params
    |> Enum.map(&Type.error_prongs(&1, :argument))
    |> List.insert_at(0, Type.error_prongs(nif.type.return, :return))
    |> List.flatten()
  end
end
