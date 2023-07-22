defmodule ZiglerTest.MakeGuides do

  defstruct lines: [], on: false

  def go do
    "guides"
    |> File.ls!
    |> Enum.each(fn filename ->
      if String.ends_with?(filename, ".md") do

      dest_filename = filename
      |> Path.basename(".md")
      |> String.replace_suffix("", "_test.exs")

      dest_stream = "test/guides"
      |> Path.join(dest_filename)
      |> File.stream!([])

      "guides"
      |> Path.join(filename)
      |> File.stream!([], :line)
      |> Enum.reduce(%__MODULE__{}, &collect_sections/2)
      |> Map.get(:lines)
      |> Enum.reverse
      |> Enum.into(dest_stream)
      end
    end)
  end

  defp collect_sections(line, acc = %{on: false}) do
    %{acc | on: String.trim(line) == "```elixir"}
  end

  defp collect_sections(line, acc) do
    if String.trim(line) == "```" do
      %{acc | on: false}
    else
      %{acc | lines: [line | acc.lines]}
    end
  end
end
