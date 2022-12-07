defmodule Zig.Nif.Synchronous do
  alias Zig.Nif
  alias Zig.Type
  alias Zig.Type.Function

  @behaviour Zig.Nif.Concurrency

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
        raise unquote(error_text)
      end
    end
  end

  require EEx

  synchronous = Path.join(__DIR__, "../templates/synchronous.zig.eex")
  EEx.function_from_file(:def, :synchronous, synchronous, [:assigns])

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
