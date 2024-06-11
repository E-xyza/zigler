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
    unionize(convert(a), convert(b))
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

  defp convert([item]) do
    quote do
      {:type, _, :list, [unquote(convert(item))]}
    end
  end

  defp convert({:<<>>, _, [{:"::", _, [{:_, _, _}, {:*, _, [{:_, _, _}, unit]}]}]}) do
    quote do
      {:type, _, :binary, [unquote(convert(0)), unquote(convert(unit))]}
    end
  end

  defp convert({:<<>>, _, [{:"::", _, [{:_, _, _}, length]}]}) do
    quote do
      {:type, _, :binary, [unquote(convert(length)), unquote(convert(0))]}
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

  defp unionize({:{}, _, [:type, _, :union, u1]}, {:{}, _, [:type, _, :union, u2]}) do
    new_union = u1 ++ u2

    quote do
      {:type, _, :union, unquote(new_union)}
    end
  end

  defp unionize({:{}, _, [:type, _, :union, u1]}, t2) do
    new_union = u1 ++ [t2]

    quote do
      {:type, _, :union, unquote(new_union)}
    end
  end

  defp unionize(t1, {:{}, _, [:type, _, :union, u2]}) do
    new_union = [t1 | u2]

    quote do
      {:type, _, :union, unquote(new_union)}
    end
  end

  defp unionize(t1, t2) do
    quote do
      {:type, _, :union, [unquote(t1), unquote(t2)]}
    end
  end
end
