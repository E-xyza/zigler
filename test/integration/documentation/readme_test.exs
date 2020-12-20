defmodule ZiglerTest.Integration.Documentation.ReadmeTest do
  use ExUnit.Case, async: true

  alias ZiglerTest.Support.Parser
  require Parser

  @project Zigler.MixProject.project

  @readme_path Parser.resource("README.md")

  @external_resource @readme_path

  @readme Parser.code_blocks("README.md")

  test "the version numbers match" do
    assert @readme |> hd |> elem(0) =~ @project[:version]
  end

  env = __ENV__

  @readme
  |> tl
  |> Enum.each(
    &Code.eval_string(
      elem(&1, 0),
      [],
      %{env | file: @readme_path, line: elem(&1, 1)}))

end
