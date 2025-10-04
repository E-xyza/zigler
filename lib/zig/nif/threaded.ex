defmodule Zig.Nif.Threaded do
  @moduledoc false

  @behaviour Zig.Nif.Concurrency

  alias Zig.ErrorProng
  alias Zig.Nif
  alias Zig.Type
  alias Zig.Parameter

  require EEx

  import Zig.QuoteErl

  @impl true
  def render_elixir(%{signature: %{arity: arity}} = nif, overrides) do
    used_params_ast = Nif.elixir_parameters(arity, true)
    unused_params_ast = Nif.elixir_parameters(arity, false)

    # TODO: marshal parameters (that need to be marshalled) here.

    function_code = [
      do:
        quote do
          ref = unquote(entrypoint(nif, :launch))(unquote_splicing(used_params_ast))

          receive do
            {:done, ^ref} ->
              unquote(entrypoint(nif, :join))(ref)

            {:error, ^ref} ->
              raise unquote("thread for function #{nif.name} failed during launch")
          end
        end
    ]

    argument_error_prong = ErrorProng.argument_error_prong(:elixir, nif.file, nif.line)

    override_error_prong =
      List.wrap(
        if arity in overrides do
          quote do
            :__nif_binding_error__ ->
              super(unquote_splicing(used_params_ast))
          end
        end
      )

    error_return_prong =
      List.wrap(
        if match?(%Zig.Type.Error{}, nif.signature.return) do
          ErrorProng.return_error_prong(:elixir, [entrypoint(nif, :join)], nif.name, nif.line)
        end
      )

    error_prongs = [catch: argument_error_prong ++ error_return_prong ++ override_error_prong]

    function_block = function_code ++ error_prongs

    launch_fn =
      if arity in overrides do
        quote do
          defoverridable [unquote({nif.name, arity})]

          defp unquote(entrypoint(nif, :launch))(unquote_splicing(unused_params_ast)) do
            throw :__nif_binding_error__
          end
        end
      else
        quote do
          defp unquote(entrypoint(nif, :launch))(unquote_splicing(unused_params_ast)) do
            :erlang.nif_error(unquote(Nif.binding_error(nif.name, arity)))
          end
        end
      end

    quote context: Elixir do
      unquote(launch_fn)

      defp unquote(entrypoint(nif, :join))(_ref) do
        :erlang.nif_error(unquote(Nif.binding_error(nif.name, arity)))
      end

      unquote(Nif.style(nif))(
        unquote(nif.name)(unquote_splicing(used_params_ast)),
        unquote(function_block)
      )
    end
  end

  @impl true
  def render_erlang(%{name: name, signature: %{arity: arity}} = nif) do
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
      launch_fail_text: ~c"thread for function #{name} failed during launch",
      error_text: ~c"nif for function #{name}/#{arity} not bound"
    )
  end

  @impl true
  threaded = Path.join(__DIR__, "../templates/threaded.zig.eex")
  EEx.function_from_file(:def, :render_zig, threaded, [:assigns])

  @impl true
  def table_entries(nif) do
    launch = entrypoint(nif, :launch)
    join = entrypoint(nif, :join)

    launch_entries = Enum.flat_map(nif.arity, &[{launch, &1, :"@\"#{launch}\"", :synchronous}])

    [{join, 1, :"@\"#{join}\"", :synchronous} | launch_entries]
  end

  defp entrypoint(nif, action) do
    :"#{nif.name}-#{action}"
  end

  @impl true
  def resources(nif), do: [{:root, :"@\"ThreadResource-#{nif.name}\""}]

  # defp error_prongs(nif) do
  #  nif.signature.params
  #  |> Enum.map(&Type.error_prongs(&1, :argument))
  #  |> List.insert_at(0, Type.error_prongs(nif.signature.return, :return))
  #  |> List.flatten()
  # end
end
