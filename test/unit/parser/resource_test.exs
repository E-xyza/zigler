defmodule ZiglerTest.Parser.ResourceTest do
  # these tests make sure that the parser can correctly identify resources.
  use ExUnit.Case, async: true

  alias Zigler.Parser
  alias Zigler.Parser.Resource

  @moduletag :parser
  @moduletag :resource

  describe "the resource definition parser" do
    test "correctly matches on a resource" do
      assert {:ok, _, _, context, _, _} = Parser.parse_resource_definition("""
        const foo = i64;
      """, context: %{local: %Resource{name: :foo}})

      assert %Parser{global: [resource], local: nil} = context
      assert %Resource{name: :foo} = resource
    end
  end

end
