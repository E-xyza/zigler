defmodule Zigler.Parser do
  def find_comments(line, so_far) do
    case String.trim(line) do
      "///" <> rest ->
        [{:comment, String.trim(rest)} | so_far]
      any -> [any | so_far]
    end
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
