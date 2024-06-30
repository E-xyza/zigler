defmodule ZiglerTest.CornerCases.TypespecOverrideTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  import ZiglerTest.SpecTemplate
  require ZiglerTest.Compiler

  setup_all do
    ZiglerTest.Compiler.compile("typespec_override.ex")
    Code.Typespec.fetch_specs(ZiglerTest.TypespecOverride)
  end

  test "it is possible to override the typespec", specs do
    assert spec((integer -> integer)) = Map.fetch!(specs, {:do_something, 1})
  end
end
