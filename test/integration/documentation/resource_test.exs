defmodule ZiglerTest.Integration.Documentation.ResourceTest do
  use ExUnit.Case, async: true

  alias ZiglerTest.Support.Parser
  require Parser

  use Zig

  resource_path = Parser.resource("guides/resources.md")
  resource = Parser.code_blocks(resource_path)
  env = __ENV__

  Enum.each(
    resource,
    &Code.eval_string(
      elem(&1, 0),
      [],
      %{env | file: resource_path, line: elem(&1, 1)}))

  @external_resource resource_path
end
