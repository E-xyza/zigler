defmodule ZiglerTest.UnitParserTest do
  use ExUnit.Case

  alias Zigler.Unit.Parser

  describe "the test header parser" do
    test "can identify test headers" do
      code = """
        test "this is a test" {
          assert 1 == 1;
        }
      """

      assert {:ok, content, _, _, _, _} = Parser.parse_test_header(code)
    end
  end
end
