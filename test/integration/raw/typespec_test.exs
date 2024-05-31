defmodule ZiglerTest.Unit.Typespec.RawTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  require ZiglerTest.Compiler

  setup_all do
    ZiglerTest.Compiler.compile("_typespec.ex")
    :ok
  end

  test "raw call with single arity has correct typespecs" do
    assert {:ok, [
      {{:raw, _},
       [
         {:type, _, :fun,
          [
            {:type, _, :product, [{:type, _, :term, _}]},
            {:type, _, :term, _}
          ]}
       ]}
    ]} = Code.Typespec.fetch_specs(ZiglerTest.Integration.Raw.Typespec)
  end
end
