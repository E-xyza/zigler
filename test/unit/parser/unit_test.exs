# TODO: UNCOMMENT THIS MODULE
#defmodule ZiglerTest.Parser.UnitTest do
#  use ExUnit.Case, async: true
#
#  @test_name "forty seven returns forty seven"
#  @test_atom String.to_atom(@test_name)
#
#  @core_doc """
#  const assert = beam.assert;
#
#  /// nif: forty_seven/0
#  fn forty_seven() i32 {
#    return 47;
#  }
#
#  test "#{@test_name}" {
#    assert(47 == forty_seven());
#  }
#  """
#
#  alias Zigler.Parser.Unit
#  alias Zigler.Parser.Nif
#
#  test "a unit test header is properly detected" do
#    hash = Zigler.Unit.name_to_hash("foobar")
#    code = "fn test_#{hash}() !void"
#    assert {:ok, [^code], "", %{tests: [%Nif{test: :foobar}]}, _, _} =
#      Unit.test_decl_parser("test \"foobar\"", context: %{file: "foo.zig"})
#  end
#
#  describe "parenthetical descriptions work" do
#    test "in the basic case" do
#      assert {:ok, data, _, _, _, _} = Unit.test_parenthetical("(foo)")
#      assert "(foo)" = IO.iodata_to_binary(data)
#    end
#    test "in the complex case" do
#      assert {:ok, data, _, _, _, _} = Unit.test_parenthetical("(fo(bar)o)")
#      assert "(fo(bar)o)" = IO.iodata_to_binary(data)
#    end
#    test "with leftovers" do
#      assert {:ok, data, rest, _, _, _} = Unit.test_parenthetical("(fo(bar)o) baz")
#      assert "(fo(bar)o)" = IO.iodata_to_binary(data)
#      assert " baz" = rest
#    end
#  end
#
#  describe "asserts are properly detected and rewritten" do
#    test "in the basic case" do
#      assert {:ok, [rewritten], _, _, _, _} = Unit.test_assert_parser("assert(1 == 1)", context: %{file: "foo.zig"})
#      assert rewritten == "try assert(1 == 1, \"foo.zig\", 1)"
#    end
#    test "with nested parentheticals" do
#      assert {:ok, [rewritten], _, _, _, _} = Unit.test_assert_parser("assert(1 == bar())", context: %{file: "foo.zig"})
#      assert rewritten == "try assert(1 == bar(), \"foo.zig\", 1)"
#    end
#  end
#
#  test "a unit test is correctly converted" do
#    {:ok, code, "", context, _, _} =
#      Unit.unit_parser(@core_doc, context: %{file: "foo.zig"})
#
#    assert [%Nif{test: @test_atom}] = context.tests
#
#    assert IO.iodata_to_binary(code) == """
#    const assert = beam.assert;
#
#    /// nif: forty_seven/0
#    fn forty_seven() i32 {
#      return 47;
#    }
#
#    fn test_#{Zigler.Unit.name_to_hash @test_name}() !void {
#      try assert(47 == forty_seven(), "foo.zig", 9);
#    }
#    """
#  end
#end
