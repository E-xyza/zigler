defmodule Zig.Parser do
  defstruct [:doc_comment]

  require Pegasus
  import NimbleParsec

  @test Mix.env() == :test

  alias Zig.Parser.Block
  alias Zig.Parser.TestDecl

  @keywords ~w(align allowzero and anyframe anytype asm async await break callconv catch comptime const continue defer else enum errdefer error export extern fn for if inline noalias nosuspend noinline opaque or orelse packed pub resume return linksection struct suspend switch test threadlocal try union unreachable usingnamespace var volatile while)a
  @keyword_mapping Enum.map(@keywords, &{:"KEYWORD_#{&1}", [token: &1]})

  @operators ~w(AMPERSAND AMPERSANDEQUAL ASTERISK ASTERISK2 ASTERISKEQUAL ASTERISKPERCENT ASTERISKPERCENTEQUAL CARET CARETEQUAL COLON COMMA DOT DOT2 DOT3 DOTASTERISK DOTQUESTIONMARK EQUAL EQUALEQUAL EQUALRARROW EXCLAMATIONMARK EXCLAMATIONMARKEQUAL LARROW LARROW2 LARROW2EQUAL LARROWEQUAL LBRACE LBRACKET LPAREN MINUS MINUSEQUAL MINUSPERCENT MINUSPERCENTEQUAL MINUSRARROW PERCENT PERCENTEQUAL PIPE PIPE2 PIPEEQUAL PLUS PLUS2 PLUSEQUAL PLUSPERCENT PLUSPERCENTEQUAL LETTERC QUESTIONMARK RARROW RARROW2 RARROW2EQUAL RARROWEQUAL RBRACE RBRACKET RPAREN SEMICOLON SLASH SLASHEQUAL TILDE)a
  @operator_mapping Enum.map(@operators, &{&1, [token: true]})

  @parser_options [
    container_doc_comment: [post_traverse: :container_doc_comment, tag: true, collect: true],
    TestDecl: [tag: TestDecl, post_traverse: {TestDecl, :post_traverse, []}, export: @test],
    skip: [ignore: true],
    STRINGLITERALSINGLE: [tag: true, post_traverse: :string_literal_single, collect: true],
    Block: [tag: Block, post_traverse: {Block, :post_traverse, []}, export: @test],
  ] ++ @keyword_mapping ++ @operator_mapping

  Pegasus.parser_from_file(Path.join(__DIR__, "grammar/grammar.y"), @parser_options)

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

  defp string_literal_single(rest, [{:STRINGLITERALSINGLE, [literal]} | rest_args], context, _, _) do
    trimmed_literal = String.trim(literal, ~S("))
    {rest, [trimmed_literal | rest_args], context}
  end

end
