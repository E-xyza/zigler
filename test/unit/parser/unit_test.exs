defmodule ZiglerTest.Parser.UnitTest do
  use ExUnit.Case, async: true

  @test_name "forty seven returns forty seven"

  @core_doc """
  const assert = beam.assert;

  /// nif: forty_seven/0
  fn forty_seven() i32 {
    return 47;
  }

  test "#{@test_name}" {
    assert(47 == 47);
  }
  """

  alias Zigler.Parser.Unit
  alias Zigler.Parser.Nif

  test "a unit test header is properly detected" do
    hash = Zigler.Unit.name_to_hash("foobar")
    code = "fn test_#{hash}() !void"
    assert {:ok, [^code], "", %{tests: [%Nif{test: "foobar"}]}, _, _} =
      Unit.test_decl_parser("test \"foobar\"")
  end


  test "a unit test is correctly converted" do
    {:ok, code, "", context, _, _} = Unit.unit_parser(@core_doc)

    assert [%Nif{test: @test_name}] = context.tests

    assert IO.iodata_to_binary(code) == """
    const assert = beam.assert;

    /// nif: forty_seven/0
    fn forty_seven() i32 {
      return 47;
    }

    fn test_#{Zigler.Unit.name_to_hash @test_name}() !void {
      assert(47 == 47);
    }
    """
  end


end
