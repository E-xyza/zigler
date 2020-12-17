defmodule ZigTest.Assembler.IncludeTest do
  use ExUnit.Case, async: true

  @moduletag :assembler

  alias Zig.Assembler

  assets_dir = __ENV__.file
  |> Path.dirname
  |> Path.join("assets")

  @target_dir "/tmp/bar"
  @basic_defaults [
    parent_dir: assets_dir,
    target_dir: @target_dir,
    context: [],
    pub: true
  ]

  test "the assembler parser outputs the correct type" do
    [_, include] = Assembler.parse_file("include1.zig", @basic_defaults)
    assert :cinclude == include.type
    assert {:cinclude, "foo.h"} == include.target
    refute include.pub
  end

  test "the assembler parser can obtain more than one c header" do
    [_, include1, include2] = Assembler.parse_file("include2.zig", @basic_defaults)
    assert :cinclude == include1.type
    assert {:cinclude, "foo.h"} == include1.target
    refute include1.pub

    assert :cinclude == include2.type
    assert {:cinclude, "bar.h"} == include2.target
    refute include2.pub
  end
end
