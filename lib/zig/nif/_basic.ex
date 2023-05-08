defmodule Zig.Nif.Basic do
  @moduledoc """
  Architecture:

  Synchronous has two different cases.  The first case is that the nif can be called
  directly.  In this case, the function is mapped directly to function name.  In the
  case that the nif needs marshalling, the function is mapped to `marshalled-<nifname>`.
  and the called function contains wrapping logic.

  To understand wrapping logic, see `Zig.Nif.Marshaller`
  """

  alias Zig.ErrorProng
  alias Zig.Nif
  alias Zig.Type

  import Zig.QuoteErl

  # marshalling setup

  defp needs_marshal?(nif) do
    Enum.any?(nif.type.params, &Type.marshals_param?/1) or
      Type.marshals_return?(nif.type.return) or
      error_prongs(nif) !== []
  end

  defp error_prongs(nif) do
    nif.type.params
    |> Enum.map(&Type.error_prongs(&1, :argument))
    |> List.insert_at(0, Type.error_prongs(nif.type.return, :return))
    |> List.flatten()
  end

  defp marshal_name(nif), do: :"marshalled-#{nif.type.name}"

  def entrypoint(nif) do
    if needs_marshal?(nif), do: marshal_name(nif), else: nif.type.name
  end

  def render_elixir(nif = %{type: type}) do
    {empty_params, used_params} =
      case type.arity do
        0 ->
          {[], []}

        n ->
          1..n
          |> Enum.map(&{{:"_arg#{&1}", [], Elixir}, {:"arg#{&1}", [], Elixir}})
          |> Enum.unzip()
      end

    error_text = "nif for function #{type.name}/#{type.arity} not bound"

    def_or_defp = if nif.export, do: :def, else: :defp

    if needs_marshal?(nif) do
      render_elixir_marshalled(nif, def_or_defp, empty_params, used_params, error_text)
    else
      quote context: Elixir do
        unquote(def_or_defp)(unquote(type.name)(unquote_splicing(empty_params))) do
          :erlang.nif_error(unquote(error_text))
        end
      end
    end
  end

  defp render_elixir_marshalled(
         nif = %{type: type},
         def_or_defp,
         empty_params,
         used_params,
         error_text
       ) do
    marshal_name = marshal_name(nif)

    error_prongs =
      nif
      |> error_prongs()
      |> Enum.flat_map(&apply(ErrorProng, &1, [:elixir]))

    marshal_params =
      type.params
      |> Enum.zip(used_params)
      |> Enum.with_index()
      |> Enum.flat_map(fn {{param_type, param}, index} ->
        List.wrap(
          if Type.marshals_param?(param_type) do
            Type.marshal_param(param_type, param, index, :elixir)
          end
        )
      end)

    return =
      quote do
        return
      end

    marshal_return =
      if Type.marshals_return?(type.return) do
        Type.marshal_return(type.return, return, :elixir)
      else
        return
      end

    quote do
      unquote(def_or_defp)(unquote(type.name)(unquote_splicing(used_params))) do
        unquote_splicing(marshal_params)
        return = unquote(marshal_name)(unquote_splicing(used_params))
        unquote(marshal_return)
      catch
        unquote(error_prongs)
      end

      defp unquote(marshal_name)(unquote_splicing(empty_params)) do
        :erlang.nif_error(unquote(error_text))
      end
    end
  end

  def render_erlang(nif = %{type: type}) do
    {unused_vars, used_vars} =
      case type.arity do
        0 ->
          {[], []}

        n ->
          1..n
          |> Enum.map(&{{:var, :"_X#{&1}"}, {:var, :"X#{&1}"}})
          |> Enum.unzip()
      end

    error_text = ~c'nif for function #{type.name}/#{type.arity} not bound'

    if needs_marshal?(nif) do
      error_prongs =
        nif
        |> error_prongs()
        |> Enum.flat_map(&apply(ErrorProng, &1, [:erlang]))

      {marshalled_vars, marshal_code} =
        type.params
        |> Enum.zip(used_vars)
        |> Enum.map_reduce([], fn {param_type, {:var, var}}, so_far ->
          if Type.marshals_param?(param_type) do
            {{:var, :"#{var}_m"}, [so_far, Type.marshal_param(param_type, var, nil, :erlang)]}
          else
            {var, so_far}
          end
        end)

      result_code =
        if Type.marshals_param?(type.return) do
          Type.marshal_return(type.return, :Result, :erlang)
        else
          "Result"
        end

      quote_erl(
        """
        unquote(function_name)(unquote(...used_vars)) ->

          #{marshal_code}

          try unquote(marshal_name)(unquote(...marshalled_vars)) of
            Result ->
              #{result_code}
          catch
            splice_prongs(error_prongs)
          end.

        unquote(marshal_name)(unquote(...unused_vars)) ->
          erlang:nif_error(unquote(error_text)).
        """,
        function_name: type.name,
        used_vars: used_vars,
        unused_vars: unused_vars,
        marshalled_vars: marshalled_vars,
        marshal_name: marshal_name(nif),
        error_text: error_text,
        error_prongs: error_prongs
      )
    else
      quote_erl(
        """
        unquote(function_name)(unquote(...vars)) ->
          erlang:nif_error(unquote(error_text)).
        """,
        function_name: type.name,
        vars: unused_vars,
        error_text: error_text
      )
    end
  end

  require EEx

  basic = Path.join(__DIR__, "../templates/basic.zig.eex")
  EEx.function_from_file(:defp, :basic, basic, [:assigns])

  basic_raw_zig = Path.join(__DIR__, "../templates/basic_raw_zig.eex")
  EEx.function_from_file(:defp, :basic_raw_zig, basic_raw_zig, [:assigns])

  def render_zig(nif = %{raw: :zig}), do: basic_raw_zig(nif)
  # note a raw "c" function does not need to have any changes made.
  def render_zig(%{raw: :c}), do: ""
  def render_zig(nif), do: basic(nif)
end
