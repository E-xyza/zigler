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

  describe "for enum specs" do
    test "atom return", specs do
      assert spec((:ok | :error | 0..1 -> :ok | :error)) = Map.fetch!(specs, {:enum_fn, 1})
    end

    test "integer return", specs do
      assert spec((:ok | :error | 0..1 -> 0..1)) = Map.fetch!(specs, {:enum_fn_integer_return, 1})
    end
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
      assert spec(
               (0..340_282_366_920_938_463_463_374_607_431_768_211_455 ->
                  0..340_282_366_920_938_463_463_374_607_431_768_211_455)
             ) =
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
    assert spec((-> :ok)) = Map.fetch!(specs, {:void_fn, 0})
  end

  describe "for arrays" do
    test "of u32", specs do
      assert spec(([0..4_294_967_295] | <<_::128>> -> [0..4_294_967_295])) =
               Map.fetch!(specs, {:u32_array_fn, 1})
    end

    test "of u32, binary return", specs do
      assert spec(([0..4_294_967_295] | <<_::128>> -> <<_::128>>)) =
               Map.fetch!(specs, {:u32_array_fn_binary_return, 1})
    end

    test "of u8, default binary", specs do
      assert spec(([byte] | <<_::32>> -> <<_::32>>)) =
               Map.fetch!(specs, {:u8_array_fn, 1})
    end

    test "of u8, force list", specs do
      assert spec(([byte] | <<_::32>> -> [byte])) =
               Map.fetch!(specs, {:u8_array_fn_list_return, 1})
    end

    test "of array, inner list", specs do
      assert spec(([[0..4_294_967_295] | <<_::128>>] | <<_::512>> -> [[0..4_294_967_295]])) =
               Map.fetch!(specs, {:array_of_arrays_fn, 1})
    end

    test "of array, inner binary", specs do
      assert spec(([[0..4_294_967_295] | <<_::128>>] | <<_::512>> -> [<<_::128>>])) =
               Map.fetch!(specs, {:array_of_arrays_fn_list_of_binary_return, 1})
    end

    test "of array, outer binary", specs do
      assert spec(([[0..4_294_967_295]] | [<<_::128>>] | <<_::512>> -> <<_::512>>)) = Map.fetch!(specs, {:array_of_arrays_fn_binary_return, 1})
    end
  end
end
