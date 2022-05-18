defmodule ZiglerTest.ParserTest do
  use ExUnit.Case, async: true

  alias Zig.Parser
  alias Zig.Parser.{Nif, Resource}

  @moduletag :parser

  describe "the zig block parser" do
    test "can correctly parse a zig block with a single nif function" do
      assert {:ok, [], "", %Parser{global: [global]}, _, _} =
               Parser.parse_zig_block("""

               /// nif: foo/0
               fn foo() i64 {
                 return 47;
               }

               """)

      assert %Nif{arity: 0, name: :foo, args: [], retval: "i64"} = global
    end

    test "can correctly parse a zig block with a multiple nif function and junk" do
      assert {:ok, [], "", %Parser{global: global}, _, _} =
               Parser.parse_zig_block("""

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

      assert Enum.any?(global, &match?(%Nif{arity: 0, name: :foo, args: [], retval: "i64"}, &1))

      assert Enum.any?(
               global,
               &match?(%Nif{arity: 2, name: :oof, args: ["i64", "f64"], retval: "i64"}, &1)
             )
    end

    test "can correctly parse a zig block with a resource section" do
      assert {:ok, [], "", %Parser{global: [global]}, _, _} =
               Parser.parse_zig_block("""

               /// resource: foo definition
               const foo = i64;

               """)

      assert %Resource{name: :foo} = global
    end

    test "can correctly match up a zig block with resource cleanup after the resource definition" do
      assert {:ok, [], "", %Parser{global: [global]}, _, _} =
               Parser.parse_zig_block("""

               /// resource: foo definition
               const foo = i64;

               /// resource: foo cleanup
               fn foo_cleanup(env: beam.env, foo_ptr: *foo) void {
                 // code that does something
               }

               """)

      assert %Resource{name: :foo, cleanup: :foo_cleanup} = global
    end

    test "can correctly match up a zig block with resource cleanup before the resource definition" do
      assert {:ok, [], "", %Parser{global: [global]}, _, _} =
               Parser.parse_zig_block("""

               /// resource: foo cleanup
               fn foo_cleanup(env: beam.env, foo_ptr: *foo) void {
                 // code that does something
               }

               /// resource: foo definition
               const foo = i64;

               """)

      assert %Resource{name: :foo, cleanup: :foo_cleanup} = global
    end

    test "will generate a resource struct for a threaded nif" do
      assert {:ok, [], "", %Parser{global: global}, _, _} =
               Parser.parse_zig_block("""

               /// nif: foo/0 threaded
               fn foo() i64 {
                 return 47;
               }

               """)

      assert Enum.any?(
               global,
               &match?(%Resource{name: :__foo_cache_ptr__, cleanup: :__foo_cache_cleanup__}, &1)
             )

      assert Enum.any?(global, &match?(%Nif{name: :foo, opts: [concurrency: :threaded]}, &1))
    end

    test "will ignore lines with four or more slashes" do
      assert {:ok, _, _, %{local: nil}, _, _} =
               Parser.parse_zig_block("""
               ////
               """)

      assert {:ok, _, _, %{local: nil}, _, _} =
               Parser.parse_zig_block("""
               /////
               """)
    end
  end

  describe "the zig code parser" do
    @empty_module %Zig.Module{file: "", module: __MODULE__, otp_app: :zigler}

    test "can correctly parse a zig block with a single nif function" do
      assert %Zig.Module{nifs: [nif]} =
               Parser.parse(
                 """

                 /// nif: foo/0
                 fn foo() i64 {
                   return 47;
                 }

                 """,
                 @empty_module,
                 "foo.ex",
                 1
               )

      assert %Nif{arity: 0, name: :foo, args: [], retval: "i64"} = nif
    end

    test "can correctly chain zig parsing events" do
      first_parse =
        Parser.parse(
          """

          /// nif: foo/0
          fn foo() i64 {
            return 47;
          }

          """,
          @empty_module,
          "foo.ex",
          1
        )

      assert %Zig.Module{nifs: nifs} =
               Parser.parse(
                 """
                 const bar = struct {
                   baz: i64,
                   quux: i64
                 };

                 /// nif: oof/2
                 fn oof(rab: i64, zab: f64) i64 {
                   return rab + 1;
                 }

                 """,
                 first_parse,
                 "foo.ex",
                 1
               )

      assert Enum.any?(nifs, &match?(%Nif{arity: 0, name: :foo, args: [], retval: "i64"}, &1))

      assert Enum.any?(
               nifs,
               &match?(%Nif{arity: 2, name: :oof, args: ["i64", "f64"], retval: "i64"}, &1)
             )
    end

    test "correctly puts content into the code argument" do
      code1 = """
      /// nif: foo/0
      fn foo() i64 {
        return 47;
      }
      """

      code2 = """
      const bar = struct {
        baz: i64,
        quux: i64
      };

      /// nif: oof/2
      fn oof(rab: i64, zab: f64) i64 {
        return rab + 1;
      }
      """

      first_parse = Parser.parse(code1, @empty_module, "foo.ex", 1)
      assert %Zig.Module{code: code} = Parser.parse(code2, first_parse, "foo.ex", 1)

      code_binary = IO.iodata_to_binary(code)

      assert code_binary =~ code1
      assert code_binary =~ code2
    end

    test "can correctly parse a zig block with a resource declaration" do
      assert %Zig.Module{resources: [resource]} =
               Parser.parse(
                 """

                 /// resource: bar definition
                 const bar = i64;

                 /// nif: foo/0
                 fn foo() i64 {
                   return 47;
                 }

                 """,
                 @empty_module,
                 "foo.ex",
                 1
               )

      assert %Resource{name: :bar} = resource
    end
  end
end
