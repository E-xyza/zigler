defmodule Zig.Nif.Synchronous do
  alias Zig.Nif
  alias Zig.Type
  alias Zig.Type.Function

  @behaviour Zig.Nif.Concurrency

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
        nif_error(unquote(error_text))
      """,
      function_name: nif.entrypoint,
      vars: vars,
      error_text: error_text
    )
  end

  require EEx

  synchronous = Path.join(__DIR__, "../templates/synchronous.zig.eex")
  EEx.function_from_file(:defp, :synchronous, synchronous, [:assigns])

  def render_zig(%Nif{function: function}), do: synchronous(function)

  def table_entries(nif) do
    [
      ~s(.{.name = "#{nif.entrypoint}", .arity = #{nif.function.arity}, .fptr = #{Function.nif_alias_for(nif.function)}, .flags = 0})
    ]
  end

  def set_entrypoint(nif = %{function: %{name: name}}) do
    entrypoint = if Nif.needs_marshal?(nif), do: :"marshalling_#{name}", else: name
    %{nif | entrypoint: entrypoint}
  end
end
