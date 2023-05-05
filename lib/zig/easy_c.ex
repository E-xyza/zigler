defmodule Zig.EasyC do
  require EEx

  easy_c = Path.join(__DIR__, "templates/easy_c.zig.eex")
  EEx.function_from_file(:def, :build_from, easy_c, [:assigns])

  def normalize_aliasing(opts) do
    if opts[:easy_c] do
      Keyword.update!(opts, :nifs, &add_aliasing/1)
    else
      opts
    end
  end

  defp add_aliasing(:auto) do
    raise "can't have `auto` on an easy_c module"
  end

  defp add_aliasing(nifs) do
    Enum.map(nifs, fn
      {:..., _, _} ->
        raise "cant have `...` on an easy_c module"

      {fun, opts} ->
        {fun, add_alias_nif(opts)}
    end)
  end

  defp add_alias_nif(opts) do
    Keyword.update(opts, :alias, true, fn
      true -> true
      alias_ -> :"easy_c.#{alias_}"
    end)
  end
end
