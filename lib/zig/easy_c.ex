defmodule Zig.EasyC do
  @moduledoc false

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

  defp add_aliasing(nifs) do
    Enum.map(nifs, fn
      {fun, opts} ->
        {fun, Keyword.update(opts, :alias, :"easy_c.#{fun}", &:"easy_c.#{&1}")}
    end)
  end
end
