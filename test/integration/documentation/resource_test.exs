defmodule ZiglerTest.Integration.Documentation.ResourceTest do
  use ExUnit.Case, async: true

  alias ZiglerTest.Support.Parser
  require Parser

  use Zig

  @readme_path Parser.resource("guides/resources.md")
  @external_resource @readme_path
  @readme Parser.code_blocks("guides/resources.md")

  env = __ENV__
  Enum.each(
    @readme,
    &Code.eval_string(
      elem(&1, 0),
      [],
      %{env | file: @readme_path, line: elem(&1, 1)}))

end
