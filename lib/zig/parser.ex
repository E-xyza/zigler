defmodule Zig.Parser do
  defstruct [:doc_comment]

  require Pegasus
  import NimbleParsec

  Pegasus.parser_from_file(Path.join(__DIR__, "grammar/grammar.y"),
    container_doc_comment: [post_traverse: :container_doc_comment, tag: true, collect: true]
  )

  defparsecp :parser, post_traverse(empty(), :init) |> parsec(:Root)

  def parse(string) do
    case parser(string) do
      {:ok, _, "", parser, _, _} -> parser
    end
  end

  # parser combinators

  defp init(code, args, context, _, _) do
    {code, args, struct(__MODULE__, context)}
  end

  defp container_doc_comment(rest, [{:container_doc_comment, [comment]} | rest_args], context, _, _) do
    doc_comment = comment
    |> String.split("\n")
    |> Enum.map(&String.trim_leading(&1, "//!"))
    |> Enum.join("\n")

    {rest, rest_args, %{context | doc_comment: doc_comment}}
  end

end
