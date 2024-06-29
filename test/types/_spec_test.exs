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

  @u38_range 0..4_294_967_295

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
      assert spec((@u38_range -> @u38_range)) = Map.fetch!(specs, {:u32_fn, 1})
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
    assert spec((@u38_range | nil -> @u38_range | nil)) =
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
      assert spec(([@u38_range] | <<_::128>> -> [@u38_range])) =
               Map.fetch!(specs, {:u32_array_fn, 1})
    end

    test "of u32, binary return", specs do
      assert spec(([@u38_range] | <<_::128>> -> <<_::128>>)) =
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
      assert spec(([[@u38_range] | <<_::128>>] | <<_::512>> -> [[@u38_range]])) =
               Map.fetch!(specs, {:array_of_arrays_fn, 1})
    end

    test "of array, inner binary", specs do
      assert spec(([[@u38_range] | <<_::128>>] | <<_::512>> -> [<<_::128>>])) =
               Map.fetch!(specs, {:array_of_arrays_fn_list_of_binary_return, 1})
    end

    test "of array, outer binary", specs do
      assert spec(([[@u38_range] | <<_::128>>] | <<_::512>> -> <<_::512>>)) =
               Map.fetch!(specs, {:array_of_arrays_fn_binary_return, 1})
    end
  end

  describe "for sentinel-terminated arrays" do
    test "the spec has variable sized binaries", specs do
      assert spec(([byte] | binary -> binary)) =
               Map.fetch!(specs, {:sentinel_terminated_array_fn, 1})
    end
  end

  describe "for c pointers" do
    @tag :skip
    test "cpointer"
  end

  describe "for manypointers" do
    test "gives the correct spec for u8", specs do
      assert spec(([byte] | binary -> byte)) = Map.fetch!(specs, {:manypointer_u8_fn, 1})
    end

    test "gives the correct spec for u32", specs do
      assert spec(([@u38_range] | <<_::_*32>> -> @u38_range)) =
               Map.fetch!(specs, {:manypointer_u32_fn, 1})
    end

    test "gives a result for sentinel-terminated u8", specs do
      assert spec((-> binary)) = Map.fetch!(specs, {:manypointer_return_fn, 0})
    end

    test "gives a result for sentinel-terminated u8 to be list", specs do
      assert spec((-> [byte])) = Map.fetch!(specs, {:manypointer_list_return_fn, 0})
    end
  end

  describe "for slices" do
    test "for f64 can be either list or binary, but returns list", specs do
      assert spec(([float] | <<_::_*64>> -> [float])) = Map.fetch!(specs, {:slice_f64_fn, 1})
    end

    test "for f64 can be either list or binary, but can be forced to return binary", specs do
      assert spec(([float] | <<_::_*64>> -> <<_::_*64>>)) =
               Map.fetch!(specs, {:slice_f64_fn_binary_return, 1})
    end

    test "for u8 can be either list or binary, but returns binary", specs do
      assert spec(([byte] | binary -> binary)) = Map.fetch!(specs, {:slice_u8_fn, 1})
    end

    test "for u8 can be either list or binary, but can be forced to return list of bytes",
         specs do
      assert spec(([byte] | binary -> [byte])) = Map.fetch!(specs, {:slice_u8_fn_list_return, 1})
    end

    test "for array of u8 can either be list or binary, but returns list", specs do
      assert spec(([[@u38_range] | <<_::96>>] | <<_::_*96>> -> [[@u38_range]])) =
               Map.fetch!(specs, {:slice_array_u32_fn, 1})
    end
  end

  describe "for normal structs" do
    test "input can be keyword or map with required", specs do
      assert spec(
               (%{value: @u38_range} | [value: @u38_range] ->
                  %{value: @u38_range})
             ) = Map.fetch!(specs, {:required_struct_fn, 1})
    end

    test "input can be keyword or map with optional, but the output is required", specs do
      assert spec(
               (%{optional(:value) => @u38_range} | [value: @u38_range] ->
                  %{value: @u38_range})
             ) = Map.fetch!(specs, {:optional_struct_fn, 1})
    end

    test "input can be binary for packed struct", specs do
      assert spec((%{value: @u38_range} | [value: @u38_range] | <<_::32>> -> <<_::32>>)) =
               Map.fetch!(specs, {:packed_struct_fn, 1})
    end

    test "output can be forced to map for packed struct", specs do
      assert spec(
               (%{value: @u38_range} | [value: @u38_range] | <<_::32>> ->
                  %{value: @u38_range})
             ) = Map.fetch!(specs, {:packed_struct_fn_map_return, 1})
    end

    test "input can be binary for extern struct", specs do
      assert spec(
               (%{value: @u38_range} | [value: @u38_range] | <<_::32>> ->
                  %{value: @u38_range})
             ) = Map.fetch!(specs, {:extern_struct_fn, 1})
    end

    test "output can be forced to map for extern struct", specs do
      assert spec((%{value: @u38_range} | [value: @u38_range] | <<_::32>> -> <<_::32>>)) =
               Map.fetch!(specs, {:extern_struct_fn_binary_return, 1})
    end

    test "you can declare internal map output", specs do
      assert spec(
               (%{value: [@u38_range] | <<_::64>>}
                | [value: [@u38_range] | <<_::64>>] ->
                  %{value: <<_::64>>})
             ) = Map.fetch!(specs, {:extern_struct_fn_internal_binary_return, 1})
    end
  end
end
