defmodule ZiglerTest.CompilerErrorParserTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser.Error

  @moduletag :parser

  describe "when passing code lines to the parser" do
    test "a non-tag line is an error" do
      assert {:error, _, _, _, _, _}
        = Error.check_ref("fn foo(x: i64) i64 {")
    end

    test "a tag line is an success" do
      assert {:ok, ["/foo/bar/test.ex", "10"], _, _, _, _} =
        Error.check_ref("// ref: /foo/bar/test.ex line: 10")
    end
  end

  @dummy_file_path "test/unit/assets/error_parser_dummy.zig"

  describe "when error parsing a dummy file" do
    test "content in the header is correctly mapped" do
      assert {@dummy_file_path, 2} = Error.backreference(@dummy_file_path, 2)
    end
    test "content in the first section is correctly mapped" do
      assert {"/path/to/foo.ex", 15} = Error.backreference(@dummy_file_path, 7)
    end
    test "content in the second section is correctly mapped" do
      assert {"/path/to/foo.ex", 48} = Error.backreference(@dummy_file_path, 14)
    end
    test "content in the footer is correctly mapped" do
      assert {@dummy_file_path, 29} = Error.backreference(@dummy_file_path, 29)
    end
  end

end
