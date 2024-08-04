defmodule Zig.QuoteErl do
  @moduledoc false
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
        {:ok, result} ->
          result

        {:error, {_, :erl_parse, reason}} ->
          raise CompileError, description: IO.iodata_to_binary(reason)
      end
    end)
  rescue
    e ->
      IO.warn("quote_erl failed with #{inspect(e)}")
      reraise e, __STACKTRACE__
  end

  defp substitute_unquoted(tokens, substitutions, so_far \\ [])

  @lparen :"("
  @rparen :")"

  # unquote a splatted value
  defp substitute_unquoted(
         [
           {:atom, _, :unquote},
           {@lparen, _},
           {:..., _},
           {:atom, line, symbol},
           {@rparen, _} | rest
         ],
         substitutions,
         so_far
       ) do
    new_so_far =
      substitutions
      |> escaped_substitution(symbol, line, splat: true)
      |> Enum.intersperse({:",", line})
      |> Enum.reverse(so_far)

    substitute_unquoted(rest, substitutions, new_so_far)
  end

  # unquote a single value
  defp substitute_unquoted(
         [{:atom, _, :unquote}, {@lparen, _}, {:atom, line, symbol}, {@rparen, _} | rest],
         substitutions,
         so_far
       ) do
    substitution = escaped_substitution(substitutions, symbol, line)
    substitute_unquoted(rest, substitutions, [substitution | so_far])
  end

  # unquote spliced prongs
  defp substitute_unquoted(
         [{:atom, _, :splice_prongs}, {@lparen, _}, {:atom, line, symbol}, {@rparen, _} | rest],
         substitutions,
         so_far
       ) do
    escaped =
      substitutions
      |> Keyword.fetch!(symbol)
      |> Enum.map(fn clause ->
        {:ok, tokens, _} =
          clause
          |> String.to_charlist()
          |> :erl_scan.string()

        tokens
      end)
      |> Enum.intersperse([{:";", line}])
      |> List.flatten()

    substitute_unquoted(rest, substitutions, Enum.reverse(escaped, so_far))
  end

  defp substitute_unquoted([head | rest], substitutions, so_far),
    do: substitute_unquoted(rest, substitutions, [head | so_far])

  defp substitute_unquoted([], _substitutions, so_far), do: Enum.reverse(so_far)

  defp escaped_substitution(substitutions, key, line, opts \\ []) do
    splat = Keyword.get(opts, :splat, false)

    case Keyword.fetch!(substitutions, key) do
      list when is_list(list) and splat ->
        Enum.map(list, &escape(&1, line))

      term ->
        escape(term, line)
    end
  end

  defp escape(atom, line) when is_atom(atom), do: {:atom, line, atom}
  defp escape(charlist, line) when is_list(charlist), do: {:string, line, charlist}
  defp escape({:var, atom}, line) when is_atom(atom), do: {:var, line, atom}
end
