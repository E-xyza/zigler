defmodule Zig.Nif.Synchronous do
  alias Zig.Nif

  def render_elixir(%Nif{type: type, function: function}) do
    params =
      case function.arity do
        0 -> []
        n -> Enum.map(1..n, &{:"_arg#{&1}", [], Elixir})
      end

    error_text = "nif for function #{function.name}/#{function.arity} not bound"

    quote context: Elixir do
      unquote(type)(unquote(function.name)(unquote_splicing(params))) do
        raise unquote(error_text)
      end
    end
  end

  require EEx

  synchronous = Path.join(__DIR__, "../templates/synchronous.zig.eex")
  EEx.function_from_file(:def, :synchronous, synchronous, [:assigns])

  def render_zig(%Nif{function: function}), do: synchronous(function)

  def arity(nif) do
    length(nif.function.params)
  end

  def table_entry(nif) do
    ~s(.{.name = "#{nif.function.name}", .arity = #{arity(nif)}, .fptr = #{nif.function.name}, .flags = 0})
  end
end
