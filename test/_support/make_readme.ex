defmodule ZiglerTest.MakeReadme do
  alias Zig.Command

  def go do
    # Read the readme document
    internal_code =
      "README.md"
      |> File.read!()
      |> Command.split_on_newline()
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

      test "version" do
        this_version = Zigler.MixProject.project()
        |> Keyword.fetch!(:version)

        [{:zigler, version_requirement, _opts}] = deps()

        assert Version.match?(this_version, version_requirement)
      end

    #{internal_code}
      
    end
    """

    File.write!("test/.readme_test.exs", templated)
  end
end
