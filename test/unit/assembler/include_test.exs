defmodule ZigTest.Assembler.IncludeTest do
  use ExUnit.Case, async: true

  @moduletag :assembler

  alias Zigler.Assembler

  @target_dir "/tmp/bar"
  @basic_defaults [
    parent_dir: assets_dir,
    target_dir: @target_dir,
    context: [],
    pub: true
  ]

  test "the assembler parser outputs the correct type" do
    [_, include] = Assembler.parse_file("includer.zig", @basic_defaults)
    assert :cinclude == include.type
    assert {:cinclude, "foo.h"} == include.target
    refute include.pub
  end
end
