defmodule ZiglerTest.SpecTemplate do
  defmacro spec([{:->, _, [params, return]}]) do
    params_ast = Enum.map(params, &convert(&1, __CALLER__))
    return_ast = convert(return, __CALLER__)

    quote do
      [{:type, _, :fun, [{:type, _, :product, unquote(params_ast)}, unquote(return_ast)]}]
    end
  end

  defp convert({name, _, atom}, _context) when is_atom(atom) do
    quote do
      {:type, _, unquote(name), []}
    end
  end

  defp convert({:@, _, _} = macro, context) do
    convert(Macro.expand(macro, context), context)
  end

  defp convert({:|, _, [a, b]}, context) do
    unionize(convert(a, context), convert(b, context))
  end

  defp convert({:%{}, [], [__struct__: Range, first: a, last: b, step: _]}, context) do
    quote do
      {:type, _, :range, [unquote(convert(a, context)), unquote(convert(b, context))]}
    end
  end

  defp convert({:.., _, [a, b]}, context) do
    quote do
      {:type, _, :range, [unquote(convert(a, context)), unquote(convert(b, context))]}
    end
  end

  defp convert({:-, _, [integer]}, context) when is_integer(integer) do
    quote do
      {:op, _, :-, unquote(convert(integer, context))}
    end
  end

  defp convert([item], context) do
    quote do
      {:type, _, :list, [unquote(convert(item, context))]}
    end
  end

  defp convert([{atom, _} | _] = kwl, context) when is_atom(atom) do
    quote do
      {:type, _, :list, [{:type, _, :union, unquote(Enum.map(kwl, &convert(&1, context)))}]}
    end
  end

  defp convert({:<<>>, _, [{:"::", _, [{:_, _, _}, {:*, _, [{:_, _, _}, unit]}]}]}, context) do
    quote do
      {:type, _, :binary, [unquote(convert(0, context)), unquote(convert(unit, context))]}
    end
  end

  defp convert({:<<>>, _, [{:"::", _, [{:_, _, _}, length]}]}, context) do
    quote do
      {:type, _, :binary, [unquote(convert(length, context)), unquote(convert(0, context))]}
    end
  end

  defp convert({:%{}, _, kv}, context) do
    quote do
      {:type, _, :map, unquote(Enum.map(kv, &convert_map(&1, context)))}
    end
  end

  defp convert({left, right}, context) do
    quote do
      {:type, _, :tuple, [unquote(convert(left, context)), unquote(convert(right, context))]}
    end
  end

  defp convert(atom, _context) when is_atom(atom) do
    quote do
      {:atom, _, unquote(atom)}
    end
  end

  defp convert(integer, _context) when is_integer(integer) do
    quote do
      {:integer, _, unquote(integer)}
    end
  end

  defp convert_map({{:optional, _, [atom]}, type}, context) when is_atom(atom) do
    quote do
      {:type, _, :map_field_assoc, [{:atom, _, unquote(atom)}, unquote(convert(type, context))]}
    end
  end

  defp convert_map({atom, type}, context) when is_atom(atom) do
    quote do
      {:type, _, :map_field_exact, [{:atom, _, unquote(atom)}, unquote(convert(type, context))]}
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
