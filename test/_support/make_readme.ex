defmodule ZiglerTest.MakeReadme do
  def go do 
    # Read the readme document
    internal_code = "README.md"
    |> File.read!()
    |> String.split("\n")
    |> Enum.reduce({false, []}, fn
      "```" <> _, {true, so_far} -> {false, so_far}
      line, {true, so_far} -> {true, [so_far, line, "\n"]}
      "```elixir" <> _, {false, so_far} -> {true, so_far}
      _, state -> state
    end)
    |> elem(1)

    templated = """
    defmodule ZiglerTest.ReadmeTest do
      use ExUnit.Case

    #{internal_code}
      
    end
    """

    File.write!("test/readme_test.exs", templated)
  end
end