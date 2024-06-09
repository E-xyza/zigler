defmodule ZiglerTest.Types.SpecTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  import ZiglerTest.SpecTemplate

  require ZiglerTest.Compiler

  setup_all do
    ZiglerTest.Compiler.compile("_spec.ex")
    Code.Typespec.fetch_specs(ZiglerTest.Types.Spec)
  end

  test "bool", types do
    assert spec([:boolean], :boolean) = Map.fetch!(types, {:bool_fn, 1})
  end
end
