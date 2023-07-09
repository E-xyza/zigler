defmodule ZiglerTest.Integration.Typespec.OverrideTest do
  use ExUnit.Case, async: true

  # NB: the code for this function is test/_support/tests

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
            ]} = Code.Typespec.fetch_specs(ZiglerTest.OverrideTypespec)
  end
end
