defmodule Zig.Parser.Case do
  defmacro assert_arg(arg, code) do
    quote do
      assert {:ok, [unquote(arg) | _], _, _, _, _} = unquote(code)
    end
  end
end
