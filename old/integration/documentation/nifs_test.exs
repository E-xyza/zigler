defmodule ZiglerTest.Integration.Documentation.NifsTest do
  # don't async this because there is a big memory allocation.
  use ExUnit.Case

  @moduletag :integration

  alias ZiglerTest.Support.Parser
  require Parser

  use Zig

  nifs_path = Parser.resource("guides/nifs.md")
  nif_code = Parser.code_blocks(nifs_path)
  env = __ENV__

  Enum.each(
    nif_code,
    &Code.eval_string(
      elem(&1, 0),
      [],
      %{env | file: nifs_path, line: elem(&1, 1)}
    )
  )

  @external_resource nifs_path
end
