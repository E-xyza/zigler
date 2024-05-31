defmodule ZiglerTest.Integration.Typespec.OverrideTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  require ZiglerTest.Compiler

  setup_all do
    ZiglerTest.Compiler.compile("_typespec_override.ex")
  end

  test "it is possible to override the typespec" do
    assert {:ok,
            [
              {{:do_something, _},
               [
                 {:type, _, :fun,
                  [
                    {:type, _, :product, [{:type, _, :integer, _}]},
                    {:type, _, :integer, _}
                  ]}
               ]}
            ]} = Code.Typespec.fetch_specs(ZiglerTest.TypespecOverride)
  end
end
