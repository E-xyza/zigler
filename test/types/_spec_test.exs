defmodule ZiglerTest.Types.SpecTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  import ZiglerTest.SpecTemplate

  require ZiglerTest.Compiler

  setup_all do
    ZiglerTest.Compiler.compile("_spec.ex")
    Code.Typespec.fetch_specs(ZiglerTest.Types.Spec)
  end

  test "bool", specs do
    assert spec((boolean -> boolean)) = Map.fetch!(specs, {:bool_fn, 1})
  end

  test "enum", specs do
    assert spec((:error | :ok -> :error | :ok)) = Map.fetch!(specs, {:enum_fn, 1})
  end

  test "float", specs do
    assert spec((float -> float)) = Map.fetch!(specs, {:float_fn, 1})
  end

  describe "for integer specs" do
    test "u5", specs do
      assert spec((0..31 -> 0..31)) = Map.fetch!(specs, {:u5_fn, 1})
    end

    test "u32", specs do
      assert spec((0..4_294_967_295 -> 0..4_294_967_295)) = Map.fetch!(specs, {:u32_fn, 1})
    end

    test "i32", specs do
      assert spec((-2_147_483_648..2_147_483_647 -> -2_147_483_648..2_147_483_647)) =
               Map.fetch!(specs, {:i32_fn, 1})
    end

    test "u128", specs do
      assert spec((0..340_282_366_920_938_463_463_374_607_431_768_211_455 ->
                   0..340_282_366_920_938_463_463_374_607_431_768_211_455)) =
               Map.fetch!(specs, {:u128_fn, 1})
    end
  end

  test "optional", specs do
    assert spec((0..4_294_967_295 | nil -> 0..4_294_967_295 | nil)) =
             Map.fetch!(specs, {:optional_fn, 1})
  end

  test "pid", specs do
    assert spec((pid -> pid)) = Map.fetch!(specs, {:pid_fn, 1})
  end

  test "term", specs do
    assert spec((term -> term)) = Map.fetch!(specs, {:term_fn, 1})
  end

  test "void", specs do
    assert spec(( -> :ok)) = Map.fetch!(specs, {:void_fn, 0})
  end
end
