defmodule Zigler.Unit.Parser do
  import NimbleParsec

  alias Zigler.Unit

  defp save_test(_rest, ["\"", title, "\"" | _], context, _, _) do
    hash_title = Unit.string_to_hash(title)
    unit = %Unit{
      title: title,
      name: hash_title
    }
    {["fn #{hash_title}() !void", unit], context}
  end

  whitespace = ascii_string([?\s, ?\n], min: 1)

  test_header =
    repeat(ascii_char([?\s]))
    |> concat(
      string("test")
      |> concat(whitespace)
      |> string("\"")
      |> utf8_string([not: ?"], min: 1)
      |> string("\"")
      |> post_traverse(:save_test))
    |> concat(whitespace)
    |> string("{")
    |> repeat(ascii_char(not: ?\n))
    |> string("\n")

  if Mix.env == :test do
    defparsec :parse_test_header, test_header
  end

  line =
    utf8_string([not: ?\n], min: 1)
    |> string("\n")
    |> reduce({Enum, :join, []})

  empty_line = string("\n")

  by_line =
    repeat(choice([
      test_header,
      line,
      empty_line
    ]))

  defparsec :test_by_line, by_line

  def get_tests(code) do
    {:ok, parsed, _, _, _, _} = code
    |> test_by_line

    {raw_code, tests} = Enum.split_with(parsed, fn
      %Unit{} -> false
      _ -> true
    end)

    %{code: raw_code, tests: tests}
  end

end
