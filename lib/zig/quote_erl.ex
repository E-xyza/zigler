defmodule Zig.QuoteErl do
  def quote_erl(quoted, substitutions \\ []) do
    # erlang separates tokenization and parsing into separate modules.
    # tokenization occurs in the erl_scan module, and this produces a tuple
    # result as follows:
    {:ok, tokens, _} =
      quoted
      |> String.to_charlist()
      |> :erl_scan.string()

    # forms, according to the erlang parser, are what you input when you're
    # compiling a .erl file (versus working with erlang in a console).  We
    # need to pre-chunk the token stream into individual 'forms' which are
    # delimited by the dot token.
    form_list =
      tokens
      |> substitute_unquoted(substitutions)
      |> Enum.chunk_while(
        [],
        fn
          dot = {:dot, _}, acc ->
            {:cont, Enum.reverse([dot | acc]), []}

          other, acc ->
            {:cont, [other | acc]}
        end,
        fn acc -> {:cont, acc} end
      )

    Enum.map(form_list, fn form ->
      case :erl_parse.parse_form(form) do
        {:ok, result} -> result
      end
    end)
  end

  defp substitute_unquoted(tokens, substitutions, so_far \\ [])

  @lparen :"("
  @rparen :")"

  defp substitute_unquoted(
         [{:atom, _, :unquote}, {@lparen, _}, {:atom, line, symbol}, {@rparen, _} | rest],
         substitutions,
         so_far
       ) do
    substitution = escaped_substitution(substitutions, symbol, line)
    substitute_unquoted(rest, substitutions, [substitution | so_far])
  end

  defp substitute_unquoted([head | rest], substitutions, so_far),
    do: substitute_unquoted(rest, substitutions, [head | so_far])

  defp substitute_unquoted([], _substitutions, so_far), do: Enum.reverse(so_far)

  defp escaped_substitution(substitutions, key, line) do
    case Access.fetch(substitutions, key)  do
      :error ->
        raise KeyError, term: substitutions, key: key

      {:ok, atom} when is_atom(atom) ->
        {:atom, line, atom}

      {:ok, charlist} when is_list(charlist) ->
        {:string, line, charlist}
    end
  end
end
