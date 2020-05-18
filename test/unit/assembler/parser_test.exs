defmodule ZigTest.Assembler.ParseTest do
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
    target_dir: @target_dir,
    context: [],
    pub: true
  ]

  test "the assembler parser outputs the correct type" do
    [empty] = Assembler.parse_file("empty.zig", @basic_defaults)
    assert :zig == empty.type
    assert empty.pub

    [empty_h] = Assembler.parse_file("empty.h", @basic_defaults)
    assert :cinclude == empty_h.type
    assert empty.pub
  end

  describe "the assembler parser creates correct paths" do
    test "for zig files" do
      [empty] = Assembler.parse_file("empty.zig", @basic_defaults)
      assert Path.join(@assets_dir, "empty.zig") == empty.source
      assert Path.join(@target_dir, "empty.zig") == empty.target
    end

    test "for c include files" do
      [empty] = Assembler.parse_file("empty.h", @basic_defaults)
      assert Path.join(@assets_dir, "empty.h") == empty.source
      assert {:include, "empty.h"} == empty.target
    end
  end

  describe "when a zig file is imported transitively" do
    test "a single file is handled correctly" do
      [importer, imported] = Assembler.parse_file("importer1.zig", @basic_defaults)

      assert %Assembler{
        context: [],
        source: Path.join(@assets_dir, "importer1.zig"),
        target: Path.join(@target_dir, "importer1.zig"),
        pub: true, type: :zig} == importer

      assert %Assembler{
        context: ["foo"],
        source: Path.join(@assets_dir, "imported1.zig"),
        target: Path.join(@target_dir, "imported1.zig"),
        pub: false, type: :zig} == imported
    end

    test "a double file is handled correctly" do
      [_, _, imported2] = Assembler.parse_file("importer2.zig", @basic_defaults)
      assert %Assembler{
        context: ["bar"],
        source: Path.join(@assets_dir, "imported2.zig"),
        target: Path.join(@target_dir, "imported2.zig"),
        pub: false, type: :zig} == imported2
    end

    test "a file in a subdirectory is handled correctly" do
      [_, imported_s] = Assembler.parse_file("importer_s.zig", @basic_defaults)

      assert %Assembler{
        context: ["foo"],
        source: Path.join(@assets_dir, "subdir/imported_s.zig"),
        target: Path.join(@target_dir, "subdir/imported_s.zig"),
        pub: false, type: :zig} == imported_s
    end

    test "transitive relations inside subdirectories are handled correctly" do
      [_, _, imported_st] = Assembler.parse_file("importer_st.zig", @basic_defaults)

      assert %Assembler{
        context: ["foo", "bar"],
        source: Path.join(@assets_dir, "subdir/transitive.zig"),
        target: Path.join(@target_dir, "subdir/transitive.zig"),
        pub: false, type: :zig} == imported_st
    end
  end

  describe "when a zig file is imported with usingnamespace" do
    test "it is handled correctly" do
      [_, imported] = Assembler.parse_file("importer_u.zig", @basic_defaults)

      assert %Assembler{
        context: [],
        source: Path.join(@assets_dir, "imported1.zig"),
        target: Path.join(@target_dir, "imported1.zig"),
        pub: false, type: :zig} == imported
    end
  end

  describe "when a zig file is imported publically" do
    test "it is tagged as public" do
      [_, imported] = Assembler.parse_file("importer_p.zig", @basic_defaults)

      assert %Assembler{
        context: ["foo"],
        source: Path.join(@assets_dir, "imported1.zig"),
        target: Path.join(@target_dir, "imported1.zig"),
        pub: true, type: :zig} == imported
    end

    test "it is not public if its parent is not private" do
      [_, _, imported] = Assembler.parse_file("importer_np.zig", @basic_defaults)

      assert %Assembler{
        context: ["foo", "foo"],
        source: Path.join(@assets_dir, "imported1.zig"),
        target: Path.join(@target_dir, "imported1.zig"),
        pub: false, type: :zig} == imported
    end
  end
end
