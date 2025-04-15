defmodule Zig.Options do
  @moduledoc false
  def quote_elixir_ast(ast) do
    Keyword.update(ast, :nifs, {:auto, []}, &quote_nifs/1)
  end

  defp quote_nifs(nifs_ast) do
    if Enum.any?(nifs_ast, &match?({:..., _, _}, &1)) do
      {:auto, normalize_elixir_ast(nifs_ast)}
    else 
      normalize_elixir_ast(nifs_ast)
    end
  end

  defp normalize_elixir_ast(ast) do
    Enum.flat_map(ast, fn
      {:..., _, _} -> []
      {fun, opts} -> [{fun, normalize_fun_opts_ast(opts)}]
      fun when is_atom(fun) -> [{fun, []}]
    end)
  end

  defp normalize_fun_opts_ast(opts) do
    Enum.map(opts, fn 
      {:spec, spec} -> {:spec, Macro.escape(spec)}
      other -> other
    end)
  end
end