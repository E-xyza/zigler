defmodule ZiglerTest.Parser.ResourceTest do
  # these tests make sure that the parser can correctly identify resources.
  use ExUnit.Case, async: true

  alias Zig.Parser
  alias Zig.Parser.Resource

  @moduletag :parser
  @moduletag :resource

  describe "the resource definition parser" do
    test "correctly matches on a resource" do
      assert {:ok, _, _, context, _, _} =
               Parser.parse_resource_definition(
                 """
                   const foo = i64;
                 """,
                 context: %{local: %Resource{name: :foo}}
               )

      assert %Parser{global: [resource], local: nil} = context
      assert %Resource{name: :foo} = resource
    end

    test "raises if the values mismatch" do
      assert_raise SyntaxError, fn ->
        Parser.parse_resource_definition(
          """
            const bar = i64;
          """,
          context: %{local: %Resource{name: :foo}}
        )
      end
    end
  end
end
