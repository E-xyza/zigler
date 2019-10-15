defmodule Zigler.Parser do

  alias Zigler.Parser.Macros
  require Macros

  # TODO: test this.
  def find_comments(line, so_far) do
    case String.trim(line) do
      "///" <> rest ->
        [{:comment, String.trim(rest)} | so_far]
      any -> [any | so_far]
    end
  end

  @spec imports([binary] | binary) :: [binary | {:absolute, binary}]
  def imports(code) when is_binary(code), do: imports([code])
  def imports(code) do
    code
    |> Enum.flat_map(&String.split(&1,"\n"))
    |> Enum.map(&Regex.run(~r/@import\(\"(.*)\"\)/, &1))
    |> Enum.flat_map(fn
      [_, v] -> [v]
      _ -> []
    end)
  end

  def stitch_strings(lst, so_far \\ [])
  def stitch_strings([line1, line2 | rest], so_far) when is_binary(line1) and is_binary(line2) do
    stitch_strings([line2 <> "\n" <> line1 | rest], so_far)
  end
  def stitch_strings([{:comment, c1}, {:comment, c2} | rest], so_far) do
    stitch_strings([{:comment, c2 <> "\n" <> c1} | rest], so_far)
  end
  def stitch_strings([any | rest], so_far) do
    stitch_strings(rest, [any | so_far])
  end
  def stitch_strings([], so_far), do: so_far

  @alphanum Macros.alphanum

  @spec tokenize(non_neg_integer, String.t) :: [any]
  def tokenize(line, string), do: tokenize(line, "", string, [])

  @spec tokenize(non_neg_integer,
    String.t | {:block, String.t, non_neg_integer},
    String.t,
    [any])
    :: [any] | {non_neg_integer, String.t, String.t, [any]}

  def tokenize(_line, "", "", lst), do: Enum.reverse(lst)
  def tokenize(_line, str, "", lst), do: Enum.reverse([str | lst])
  def tokenize(line, "", "{" <> rest, lst), do: tokenize(line, {:block, "", 1}, rest, lst)
  def tokenize(line, {:block, inner, 1}, "}" <> rest, lst) do
    block_lines = String.split(inner, "\n") |> Enum.count()
    tokenize(line + block_lines - 1, "", rest, [{:block, line, inner} | lst])
  end
  def tokenize(line, {:block, inner, n}, "{" <> rest, lst), do: tokenize(line, {:block, inner <> "{", n + 1}, rest, lst)
  def tokenize(line, {:block, inner, n}, "}" <> rest, lst), do: tokenize(line, {:block, inner <> "}", n - 1}, rest, lst)
  def tokenize(line, {:block, inner, n}, <<v>> <> rest, lst), do: tokenize(line, {:block, inner <> <<v>>, n}, rest, lst)
  def tokenize(line, "",  " " <> rest, lst),    do: tokenize(line, "", rest, lst)
  def tokenize(line, "",  "\t" <> rest, lst),   do: tokenize(line, "", rest, lst)
  def tokenize(line, "",  "\n" <> rest, lst),   do: tokenize(line + 1, "", rest, lst)
  def tokenize(line, "",  "." <> rest, lst),    do: tokenize(line, "", rest, [:. | lst])
  def tokenize(line, str, " " <> rest, lst),    do: tokenize(line, "", rest, [str | lst])
  def tokenize(line, str, "\t" <> rest, lst),   do: tokenize(line, "", rest, [str | lst])
  def tokenize(line, str, "\n" <> rest, lst),   do: tokenize(line + 1, "", rest, [str | lst])
  def tokenize(line, str, "." <> rest, lst),    do: tokenize(line, "", rest, [:., str | lst])
  def tokenize(line, "", ";" <> rest, lst),     do: tokenize(line, "", rest, [{:line, line} | lst])
  def tokenize(line, str, ";" <> rest, lst),    do: tokenize(line, "", rest, [{:line, line}, str | lst])
  Macros.tokenizers_for(~w(i8 i16 i32 i64 f16 f32 f64 fn test @import @nif))
  def tokenize(line, "",  "[]" <> rest, lst),   do: sliceize(line, rest, lst)
  def tokenize(line, "",  "[*c]" <> rest, lst), do: cstrize(line, rest, lst)
  def tokenize(line, "",  "(" <> rest, lst) do
    case tokenize(line, rest) do
      {new_line, ")", rest, interior} -> tokenize(new_line, "", rest, [Enum.reverse(interior) | lst])
      _ -> raise "parsing error, unclosed parenthesis"
    end
  end
  def tokenize(line, str, "(" <> rest, lst), do: tokenize(line, "", "(" <> rest, [str | lst])
  def tokenize(line, "", ")" <> rest, lst), do: {line, ")", rest, lst}
  def tokenize(line, str, ")" <> rest, lst), do: {line, ")",  rest, [str | lst]}
  def tokenize(line, "", "///" <> rest, lst), do: doc_string(line, "", rest, lst)
  def tokenize(line, "", "//" <> rest, lst), do: line_comment(line, rest, lst)
  def tokenize(line, str, "//" <> rest, lst), do: line_comment(line, rest, [str | lst])
  def tokenize(line, "", "/*" <> rest, lst), do: block_comment(line, rest, lst)
  def tokenize(line, str, "/*" <> rest, lst), do: block_comment(line, rest, [str | lst])
  def tokenize(line, "", "\"" <> rest, lst) do
    {interior, rest} = stringize(rest)
    tokenize(line, "", rest, [{:string, interior} | lst])
  end
  def tokenize(line, str, <<v>> <> rest, lst),  do: tokenize(line, str <> <<v>>, rest, lst)

  def sliceize(line, " " <> rest, lst),   do: sliceize(line, rest, lst)
  def sliceize(line, "\t" <> rest, lst),  do: sliceize(line, rest, lst)
  def sliceize(line, "u8" <> rest, lst),  do: tokenize(line, "", rest, [:string | lst])
  def sliceize(line, "i8" <> rest, lst),  do: tokenize(line, "", rest, [{:slice, :i8} | lst])
  def sliceize(line, "i16" <> rest, lst), do: tokenize(line, "", rest, [{:slice, :i16} | lst])
  def sliceize(line, "i32" <> rest, lst), do: tokenize(line, "", rest, [{:slice, :i32} | lst])
  def sliceize(line, "i64" <> rest, lst), do: tokenize(line, "", rest, [{:slice, :i64} | lst])
  def sliceize(line, "f16" <> rest, lst), do: tokenize(line, "", rest, [{:slice, :f16} | lst])
  def sliceize(line, "f32" <> rest, lst), do: tokenize(line, "", rest, [{:slice, :f32} | lst])
  def sliceize(line, "f64" <> rest, lst), do: tokenize(line, "", rest, [{:slice, :f64} | lst])

  def cstrize(line, "u8" <> rest, lst), do: tokenize(line, "", rest, [:cstring | lst])

  def stringize(str) do
    case String.split(str, "\"", parts: 2) do
      [interior, rest] -> {interior, rest}
      _ -> raise "error parsing, missing string close"
    end
  end

  def line_comment(line, "\n" <> rest, lst), do: tokenize(line + 1, "", rest, lst)
  def line_comment(line, <<_>> <> rest, lst), do: line_comment(line, rest, lst)

  def block_comment(line, "\n" <> rest, lst), do: block_comment(line + 1, rest, lst)
  def block_comment(line, "*/" <> rest, lst), do: tokenize(line, "", rest, lst)
  def block_comment(line, <<_>> <> rest, lst), do: block_comment(line, rest, lst)
  def block_comment(_, _, _, _), do: raise "error parsing, missing block comment close"

  def doc_string(line, docline, "\n" <> rest, [{:docstring, old} | lst]) do
    tokenize(line + 1, "", rest, [{:docstring, old <> docline <> "\n"} | lst])
  end
  def doc_string(line, docline, "\n" <> rest, lst) do
    tokenize(line + 1, "", rest, [{:docstring, docline <> "\n"} | lst])
  end
  def doc_string(line, docline, <<v>> <> rest, lst) do
    doc_string(line, docline <> <<v>>, rest, lst)
  end
end
