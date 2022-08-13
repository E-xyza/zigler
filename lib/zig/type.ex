defprotocol Zig.Type do
  alias Zig.Type.Integer

  @type t :: Integer.t

  import Kernel

  defmacro sigil_t(string, _) do
    string
    |> Code.eval_quoted([], __CALLER__)
    |> elem(0)
    |> parse
    |> Macro.escape()
  end

  def parse(string) do
    case string do
      "u" <> _ ->
        Integer.parse(string)
      "i" <> _ ->
        Integer.parse(string)
    end
  end

  defmacro __using__(_) do
    module = __CALLER__.module
    quote do
      import Inspect.Algebra
      import Kernel, except: [to_string: 1]

      defimpl String.Chars do
        defdelegate to_string(type), to: unquote(module)
      end

      defimpl Inspect do
        defdelegate inspect(type, opts), to: unquote(module)
      end
    end
  end
end
