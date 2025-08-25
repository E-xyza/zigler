defmodule ZiglerTest.CornerCases.ReturnspecOverrideTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  import ZiglerTest.SpecTemplate
  require ZiglerTest.Compiler

  setup_all do
    ZiglerTest.Compiler.compile("returnspec_override.ex")
    Code.Typespec.fetch_specs(ZiglerTest.ReturnspecOverride)
  end

  test "it is possible to override the typespec", specs do
    assert spec((0..4_294_967_295 -> {:ok, integer} | :error)) =
             Map.fetch!(specs, {:do_something, 1})
  end
end
