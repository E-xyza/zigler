defmodule Zig.Nif.Synchronous do
  alias Zig.Nif
  alias Zig.Type

  @behaviour Zig.Nif.Concurrency

  def render_elixir(%Nif{type: type, entrypoint: entrypoint, function: function}) do
    params =
      case function.arity do
        0 -> []
        n -> Enum.map(1..n, &{:"_arg#{&1}", [], Elixir})
      end

    error_text = "nif for function #{entrypoint}/#{function.arity} not bound"

    quote context: Elixir do
      unquote(type)(unquote(entrypoint)(unquote_splicing(params))) do
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
      ~s(.{.name = "#{nif.entrypoint}", .arity = #{nif.function.arity}, .fptr = #{nif.function.name}, .flags = 0})
    ]
  end

  def set_entrypoint(nif = %{function: %{name: name}}) do
    entrypoint = if Nif.needs_marshal?(nif), do: :"marshalling_#{name}", else: name
    %{nif | entrypoint: entrypoint}
  end
end
