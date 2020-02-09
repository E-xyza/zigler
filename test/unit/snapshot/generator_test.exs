defmodule ZiglerTest.GeneratorTest do
  use ExUnit.Case, async: true

  # tests to make sure that the compiler makse the correct code.

  alias Zigler.{Module, Zig, Parser.Nif}

  @zeroarity %Nif{name: :foo, arity: 0, params: [], retval: "i64"}

  describe "the generator creates a reasonable shim" do
    test "for a single, zero arity function" do
      assert """
      """ = Zig.generate(%Module{nifs: [@zeroarity]})
    end
  end
end
