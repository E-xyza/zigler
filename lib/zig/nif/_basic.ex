defmodule Zig.Nif.Basic do
  @moduledoc false

  alias Zig.ErrorProng
  alias Zig.Nif
  alias Zig.Nif.DirtyCpu
  alias Zig.Nif.DirtyIo
  alias Zig.Nif.Synchronous
  alias Zig.Parameter
  alias Zig.Return
  alias Zig.Type
  alias Zig.Type.Error
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

  defp marshals_return?(%Error{}), do: true
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

  def render_elixir(%{raw: raw} = nif, overrides) when not is_nil(raw) do
    Enum.map(nif.arity, fn arity ->
      basic_function_quoted(nif, arity, arity in overrides)
    end)
  end

  def render_elixir(%{signature: %{arity: arity}} = nif, overrides) do
    if needs_marshal?(nif) do
      render_elixir_marshalled(nif, overrides)
    else
      basic_function_quoted(nif, arity, arity in overrides)
    end
  end

  defp render_elixir_marshalled(%{signature: %{arity: arity}} = nif, overrides) do
    used_params_ast = Nif.elixir_parameters(arity, true)
    unused_params_ast = Nif.elixir_parameters(arity, false)

    marshal_name = marshal_name(nif)

    marshal_params =
      nif.params
      |> Enum.zip(used_params_ast)
      |> Enum.flat_map(fn {{index, param}, ast} ->
        List.wrap(
          if marshals_param?(param.type) do
            Type.marshal_param(param.type, ast, index, Elixir)
          end
        )
      end)

    return_ast =
      quote do
        return
      end

    marshal_return =
      if marshals_return?(nif.return.type) do
        Type.marshal_return(nif.return.type, return_ast, Elixir)
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
          ErrorProng.return_error_prong(:elixir, [marshal_name], nif.name, nif.line)
        end
      )

    override_error_prong =
      List.wrap(
        if arity in overrides do
          quote do
            :__nif_binding_error__ ->
              super(unquote_splicing(used_params_ast))
          end
        end
      )

    error_prongs = [catch: argument_error_prong ++ error_return_prong ++ override_error_prong]

    function_block = function_code ++ error_prongs

    if arity in overrides do
      quote context: Elixir do
        defoverridable [{unquote(nif.name), unquote(arity)}]

        unquote(Nif.style(nif))(
          unquote(nif.name)(unquote_splicing(used_params_ast)),
          unquote(function_block)
        )

        defp unquote(marshal_name)(unquote_splicing(unused_params_ast)) do
          throw :__nif_binding_error__
        end
      end
    else
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
  end

  defp basic_function_quoted(nif, arity, override) do
    if override do
      []
    else
      unused_params = Nif.elixir_parameters(arity, false)

      quote context: Elixir do
        unquote(Nif.style(nif))(unquote(nif.name)(unquote_splicing(unused_params))) do
          :erlang.nif_error(unquote(Nif.binding_error(nif.name, arity)))
        end
      end
    end
  end

  def render_erlang(%{signature: signature} = nif) do
    {unused_vars, used_vars} =
      case signature.arity do
        0 ->
          {[], []}

        n ->
          1..n
          |> Enum.map(&{{:var, :"_X#{&1}"}, {:var, :"X#{&1}"}})
          |> Enum.unzip()
      end

    error_text = ~c'nif for function #{nif.name}/#{signature.arity} not bound'

    if needs_marshal?(nif) do
      {marshalled_vars, marshal_code} =
        signature.params
        |> Enum.zip(used_vars)
        |> Enum.map_reduce([], fn {param_type, {:var, var}}, so_far ->
          if marshals_param?(param_type) do
            {{:var, :"#{var}_m"}, [so_far, Type.marshal_param(param_type, var, nil, :erlang)]}
          else
            {{:var, var}, so_far}
          end
        end)

      result_code =
        if marshals_param?(signature.return) do
          Type.marshal_return(signature.return, :Return, :erlang)
        else
          "Return"
        end

      argument_error_prong = ErrorProng.argument_error_prong(:erlang)

      error_return_prong =
        List.wrap(
          if match?(%Zig.Type.Error{}, nif.signature.return) do
            ErrorProng.return_error_prong(:erlang, [], nif.name, nif.line)
          end
        )

      error_prongs = argument_error_prong ++ error_return_prong

      quote_erl(
        """
        unquote(function_name)(unquote(...used_vars)) ->

          #{marshal_code}

          try unquote(marshal_name)(unquote(...marshalled_vars)) of
            Return ->
              #{result_code}
          catch
            splice_prongs(error_prongs)
          end.

        unquote(marshal_name)(unquote(...unused_vars)) ->
          erlang:nif_error(unquote(error_text)).
        """,
        function_name: nif.name,
        used_vars: used_vars,
        unused_vars: unused_vars,
        marshalled_vars: marshalled_vars,
        marshal_name: marshal_name(nif),
        error_prongs: error_prongs,
        error_text: error_text
      )
    else
      quote_erl(
        """
        unquote(function_name)(unquote(...vars)) ->
          erlang:nif_error(unquote(error_text)).
        """,
        function_name: signature.name,
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
