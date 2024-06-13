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
  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Type
  alias Zig.Type.Integer

  import Zig.QuoteErl

  # marshalling setup
  #
  # for a basic function, a marshalling function is required unless the function ONLY
  # has term or erl_nif_term parameters, because those parameters may emit `argumenterror`
  # which needs to be trapped and converted.
  #
  # returning a value cannot error so they do not need to be marshalled, except for
  # integers of size greater than 64 bits.

  defp marshals_param?(:term), do: false
  defp marshals_param?(:erl_nif_term), do: false
  defp marshals_param?(_), do: true

  defp marshals_return?(%Integer{bits: bits}), do: bits > 64
  defp marshals_return?(_), do: false

  defp needs_marshal?(%{raw: raw}) when not is_nil(raw), do: false

  defp needs_marshal?(nif) do
    Enum.any?(nif.signature.params, &marshals_param?/1) or marshals_return?(nif.signature.return)
  end

  defp marshal_name(nif), do: :"marshalled-#{nif.name}"

  def entrypoint(nif) do
    if needs_marshal?(nif), do: marshal_name(nif), else: nif.name
  end

  def render_elixir(%{raw: raw} = nif) when not is_nil(raw) do
    Enum.map(nif.params, fn arity ->
      unused_params = Nif.elixir_parameters(arity, false)

      quote do
        unquote(Nif.style(nif))(unquote(nif.name)(unquote_splicing(unused_params))) do
          :erlang.nif_error(unquote(Nif.binding_error(nif.name, arity)))
        end
      end
    end)
  end

  def render_elixir(%{signature: %{arity: arity}} = nif) do
    if needs_marshal?(nif) do
      render_elixir_marshalled(nif)
    else
      unused_params = Nif.elixir_parameters(arity, false)

      quote context: Elixir do
        unquote(Nif.style(nif))(unquote(nif.name)(unquote_splicing(unused_params))) do
          :erlang.nif_error(unquote(Nif.binding_error(nif.name, arity)))
        end
      end
    end
  end

  defp render_elixir_marshalled(%{signature: %{arity: arity}} = nif) do
    used_params_ast = Nif.elixir_parameters(arity, true)
    unused_params_ast = Nif.elixir_parameters(arity, false)

    marshal_name = marshal_name(nif)

    marshal_params =
      nif.params
      |> Enum.zip(used_params_ast)
      |> Enum.flat_map(fn {{index, param}, ast} ->
        List.wrap(
          if marshals_param?(param.type) do
            Type.marshal_param(param.type, ast, index, :elixir)
          end
        )
      end)

    return_ast =
      quote do
        return
      end

    marshal_return =
      if marshals_return?(nif.return.type) do
        Type.marshal_return(nif.return.type, return_ast, :elixir)
      else
        return_ast
      end

    function_code = [
      do:
        quote do
          unquote_splicing(marshal_params)
          return = unquote(marshal_name)(unquote_splicing(used_params_ast))
          unquote(marshal_return)
        end
    ]

    argument_error_prong = ErrorProng.argument_error_prong(:elixir, nif.file, nif.line)

    error_return_prong =
      List.wrap(
        if match?(%Zig.Type.Error{}, nif.signature.return) do
          ErrorProng.return_error_prong(:elixir, [marshal_name])
        end
      )

    error_prongs =
      case {argument_error_prong, error_return_prong} do
        {[], []} -> []
        _ -> [catch: argument_error_prong ++ error_return_prong]
      end

    function_block = function_code ++ error_prongs

    quote context: Elixir do
      unquote(Nif.style(nif))(
        unquote(nif.name)(unquote_splicing(used_params_ast)),
        unquote(function_block)
      )

      defp unquote(marshal_name)(unquote_splicing(unused_params_ast)) do
        :erlang.nif_error(unquote(Nif.binding_error(nif.name, arity)))
      end
    end
  end

  def render_erlang(%{type: type} = nif) do
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
      {marshalled_vars, marshal_code} =
        type.params
        |> Enum.zip(used_vars)
        |> Enum.map_reduce([], fn {param_type, {:var, var}}, so_far ->
          if marshals_param?(param_type) do
            {{:var, :"#{var}_m"}, [so_far, Type.marshal_param(param_type, var, nil, :erlang)]}
          else
            {{:var, var}, so_far}
          end
        end)

      result_code =
        if marshals_param?(type.return) do
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
        error_text: error_text
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

  raw_beam_term = Path.join(__DIR__, "../templates/raw_beam_term.zig.eex")
  EEx.function_from_file(:defp, :raw_beam_term, raw_beam_term, [:assigns])

  raw_erl_nif_term = Path.join(__DIR__, "../templates/raw_erl_nif_term.zig.eex")
  EEx.function_from_file(:defp, :raw_erl_nif_term, raw_erl_nif_term, [:assigns])

  def render_zig(%{raw: :term} = nif), do: raw_beam_term(nif)
  def render_zig(%{raw: :erl_nif_term} = nif), do: raw_erl_nif_term(nif)
  def render_zig(nif), do: basic(nif)

  def context(DirtyCpu), do: :dirty
  def context(DirtyIo), do: :dirty
  def context(Synchronous), do: :synchronous

  def resources(_), do: []
end
