defmodule Zigler.Parser do

  # TODO: test this:
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
end
