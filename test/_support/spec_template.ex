defmodule ZiglerTest.SpecTemplate do
  defmacro spec([{:->, _, [params, return]}]) do
    params_ast = Enum.map(params, &convert/1)
    return_ast = convert(return)

    quote do
      [{:type, _, :fun, [{:type, _, :product, unquote(params_ast)}, unquote(return_ast)]}]
    end
  end

  defp convert({name, _, atom}) when is_atom(atom) do
    quote do
      {:type, _, unquote(name), []}
    end
  end

  defp convert({:|, _, [a, b]}) do
    quote do
      {:type, _, :union, [unquote(convert(a)), unquote(convert(b))]}
    end
  end

  defp convert({:.., _, [a, b]}) do
    quote do
      {:type, _, :range, [unquote(convert(a)), unquote(convert(b))]}
    end
  end

  defp convert({:-, _, [integer]}) when is_integer(integer) do
    quote do
      {:op, _, :-, unquote(convert(integer))}
    end
  end

  defp convert(atom) when is_atom(atom) do
    quote do
      {:atom, _, unquote(atom)}
    end
  end

  defp convert(integer) when is_integer(integer) do
    quote do
      {:integer, _, unquote(integer)}
    end
  end
end
