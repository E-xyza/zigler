defmodule ZiglerTest.ParserTest do
  use ExUnit.Case, async: true

  alias Zigler.Parser
  alias Zigler.Parser.Nif

  @moduletag :parser

  describe "the zig block parser" do
    test "can correctly parse a zig block with a single nif function" do
      assert {:ok, [], "", %Parser{global: [global]}, _, _} = Parser.parse_zig_block("""

      /// nif: foo/0
      fn foo() i64 {
        return 47;
      }

      """)

      assert %Nif{arity: 0, name: :foo, params: [], retval: "i64"} = global
    end

    test "can correctly parse a zig block with a multiple nif function and junk" do
      assert {:ok, [], "", %Parser{global: global}, _, _} = Parser.parse_zig_block("""

      /// nif: foo/0
      fn foo() i64 {
        return 47;
      }

      const bar = struct {
        baz: i64,
        quux: i64
      };

      /// nif: oof/2
      fn oof(rab: i64, zab: f64) i64 {
        return rab + 1;
      }

      """)

      assert Enum.any?(global, &match?(%Nif{arity: 0, name: :foo, params: [], retval: "i64"}, &1))
      assert Enum.any?(global, &match?(%Nif{arity: 2, name: :oof, params: ["i64", "f64"], retval: "i64"}, &1))
    end
  end

  describe "the zig code parser" do

    @empty_module %Zigler.Module{file: ""}

    test "can correctly parse a zig block with a single nif function" do
      assert %Zigler.Module{nifs: [nif]} = Parser.parse("""

      /// nif: foo/0
      fn foo() i64 {
        return 47;
      }

      """, @empty_module)

      assert %Nif{arity: 0, name: :foo, params: [], retval: "i64"} = nif
    end

    test "can correctly chain zig parsing events" do
      first_parse = Parser.parse("""

      /// nif: foo/0
      fn foo() i64 {
        return 47;
      }

      """, @empty_module)

      assert %Zigler.Module{nifs: nifs} = Parser.parse("""
      const bar = struct {
        baz: i64,
        quux: i64
      };

      /// nif: oof/2
      fn oof(rab: i64, zab: f64) i64 {
        return rab + 1;
      }

      """, first_parse)

      assert Enum.any?(nifs, &match?(%Nif{arity: 0, name: :foo, params: [], retval: "i64"}, &1))
      assert Enum.any?(nifs, &match?(%Nif{arity: 2, name: :oof, params: ["i64", "f64"], retval: "i64"}, &1))
    end
  end
end
