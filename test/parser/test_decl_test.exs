defmodule ZiglerTest.Parser.FileTest do
  use ExUnit.Case, async: true

  alias Zig.Parser
  alias Zig.Parser.Block
  alias Zig.Parser.TestDecl
  import NimbleParsec
  import Zig.Parser.Case

  defparsecp :parser, parsec({Parser, :TestDecl})

  describe "the testdecl parser can" do
    test "parse an unnamed test" do
      assert_arg(%TestDecl{
        name: nil,
        code: %Block{code: []}
      }, parser(~S(test {})))
    end

    test "parse a named test" do
      assert_arg(%TestDecl{
        name: "foobar",
        code: %Block{code: []}
      }, parser(~S(test "foobar" {})))
    end
  end
end
