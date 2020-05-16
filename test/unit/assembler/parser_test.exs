defmodule ZigTest.Assembler.ParserTest do
  use ExUnit.Case, async: true

  @moduletag :assembler

  alias Zigler.Assembler

  assets_dir = __ENV__.file
  |> Path.dirname
  |> Path.join("assets")

  @assets_dir assets_dir
  @target_dir "/tmp/bar"
  @basic_defaults [
    parent_dir: assets_dir,
    target_dir: @target_dir
  ]

  test "the assembler parser outputs the correct type" do
    [empty] = Assembler.parse("empty.zig", @basic_defaults)
    assert :zig == empty.type

    [empty_h] = Assembler.parse("empty.h", @basic_defaults)
    assert :cinclude == empty_h.type
  end

  describe "the assembler parser creates correct paths" do
    test "for zig files" do
      [empty] = Assembler.parse("empty.zig", @basic_defaults)
      assert Path.join(@assets_dir, "empty.zig") == empty.source
      assert Path.join(@target_dir, "empty.zig") == empty.target
    end

    test "for c include files" do
      [empty] = Assembler.parse("empty.h", @basic_defaults)
      assert Path.join(@assets_dir, "empty.h") == empty.source
      assert {:include, "empty.h"} == empty.target
    end
  end

end
