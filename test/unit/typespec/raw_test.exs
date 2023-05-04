defmodule ZiglerTest.Unit.Typespec.RawTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  alias Zig.Type
  alias Zig.Type.Function
  alias Zig.Type.Manypointer

  import Type, only: :macros

  describe "when asking for a typespec for a raw function" do
    test "it returns ambiguous terms" do
      result =
        quote context: Elixir do
          raw_test(term(), term()) :: term()
        end

      assert Function.spec(%Function{
               name: :raw_test,
               arity: 2,
               params: [
                 :env,
                 ~t(i32),
                 %Manypointer{child: :term}
               ],
               return: :term,
               opts: [raw: true, return: :default]
             }) == result
    end
  end
end
