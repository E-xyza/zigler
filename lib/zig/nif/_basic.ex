defmodule Zig.Nif.Basic do
  @moduledoc """
  Architecture:

  Synchronous has two different cases.  The first case is that the nif can be called
  directly.  In this case, the function is mapped directly to function name.  In the
  case that the nif needs marshalling, the function is mapped to `marshalled-<nifname>`.
  and the called function contains wrapping logic.

  To understand wrapping logic, see `Zig.Nif.Marshaller`
  """

  alias Zig.Nif
  alias Zig.Type
  alias Zig.Type.Function

  import Zig.QuoteErl

  def render_elixir(nif = %{function: function}) do
    params =
      case function.arity do
        0 -> []
        n -> Enum.map(1..n, &{:"_arg#{&1}", [], Elixir})
      end

    error_text = "nif for function #{nif.entrypoint}/#{function.arity} not bound"

    type = if Nif.needs_marshal?(nif), do: :defp, else: nif.type

    quote context: Elixir do
      unquote(type)(unquote(nif.entrypoint)(unquote_splicing(params))) do
        :erlang.nif_error(unquote(error_text))
      end
    end
  end

  def render_erlang(nif = %{function: function}) do
    vars =
      case function.arity do
        0 -> []
        n -> Enum.map(1..n, &{:var, :"_X#{&1}"})
      end

    error_text = ~c'nif for function #{nif.entrypoint}/#{function.arity} not bound'

    quote_erl(
      """
      unquote(function_name)(unquote(...vars)) ->
        erlang:nif_error(unquote(error_text)).
      """,
      function_name: nif.entrypoint,
      vars: vars,
      error_text: error_text
    )
  end

  require EEx

  basic = Path.join(__DIR__, "../templates/basic.zig.eex")
  EEx.function_from_file(:defp, :basic, basic, [:assigns])

  basic_raw_zig = Path.join(__DIR__, "../templates/basic_raw_zig.eex")
  EEx.function_from_file(:defp, :basic_raw_zig, basic_raw_zig, [:assigns])

  def render_zig(%{type: function, raw: :zig}), do: basic_raw_zig(function)
  # note a raw "c" function does not need to have any changes made.
  def render_zig(%{type: function, raw: :c}), do: ""
  def render_zig(%{type: function}), do: basic(function)

  def set_entrypoint(nif = %{function: %{name: name}}) do
    entrypoint = if Nif.needs_marshal?(nif), do: :"marshalled-#{name}", else: name
    %{nif | entrypoint: entrypoint}
  end
end
