defmodule ZiglerTest.Unit.Typespec.RawTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  alias Zig.Nif
  alias Zig.Type
  alias Zig.Type.Function
  alias Zig.Type.Manypointer
  alias ZiglerTest.Spec

  import Type, only: :macros

  describe "when asking for a typespec for a raw function" do
    test "it returns ambiguous terms" do
      result =
        quote context: Elixir do
          raw_test(term(), term()) :: term()
        end

      assert Spec.for(
               %Function{
                 name: :raw_test,
                 arity: 2,
                 params: [
                   :env,
                   ~t(i32),
                   %Manypointer{child: :term}
                 ],
                 return: :term
               },
               raw: :zig
             ) == result
    end
  end
end
