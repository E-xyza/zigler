defmodule Zigler.Parser.Macros do
  defmacro tokenizers_for(lst) do
    token_list = Macro.expand(lst, __CALLER__)
    token_fns = Enum.map(token_list, &tokenizer/1)
    quote do
      unquote_splicing(token_fns)
    end
  end

  def alphanum do
    [?a..?z, ?A..?Z, ?0..?9, '_'] |> Enum.flat_map(&Enum.map(&1, fn x -> <<x>> end))
  end

  defp tokenizer(whole = "@" <> str), do: tokenizer(whole, str)
  defp tokenizer(whole, part \\ nil) do
    part = part || whole
    atm = String.to_atom(part)
    quote do
      def tokenize(line, "",  unquote(whole) <> <<x::binary-size(1)>> <> rest, lst)
        when x not in @alphanum,
        do: tokenize(line, "", x <> rest, [unquote(atm) | lst])

      def tokenize(line, "",  unquote(whole), lst),
        do: Enum.reverse([unquote(atm) | lst])
    end
  end
end
