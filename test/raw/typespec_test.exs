defmodule ZiglerTest.Raw.TypespecTest do
  use ExUnit.Case, async: true

  @moduletag :typespec
  @moduletag :skip

  import ZiglerTest.SpecTemplate
  require ZiglerTest.Compiler

  setup_all do
    ZiglerTest.Compiler.compile("typespec.ex")

    Code.Typespec.fetch_specs(ZiglerTest.Raw.Typespec)
  end

  test "raw call with single arity has correct typespecs", specs do
    assert spec((term -> term)) = specs[{:raw, 1}]
  end

  test "raw call with multi arity has correct typespecs", specs do
    assert spec((term -> term)) = specs[{:multi_raw, 1}]
    assert spec((term, term, term -> term)) = specs[{:multi_raw, 3}]
    assert spec((term, term, term, term -> term)) = specs[{:multi_raw, 4}]

    refute Map.has_key?(specs, {:multi_raw, 0})
    refute Map.has_key?(specs, {:multi_raw, 2})
    refute Map.has_key?(specs, {:multi_raw, 5})
  end
end
