defmodule ZiglerTest.SpecTemplate do
  defmacro spec(params, return) do
    params_ast = Enum.map(params, &quote do {:type, _, unquote(&1), []} end)
    return_ast = quote do {:type, _, unquote(return), []} end
    quote do
      [{:type, _, :fun, [{:type, _, :product, unquote(params_ast)}, unquote(return_ast)]}]
    end
  end
end