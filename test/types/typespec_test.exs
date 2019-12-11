defmodule ZigTest.TypespecTest do
  use ExUnit.Case, async: true

  @moduletag :one

  describe "zigler correctly creates typespecs" do
    test "for degenerate" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:zero_arity, 0, [], :integer) in types
      assert typespec_for(:void_return, 0, [], nil) in types
    end

    test "for integers" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:u8_identity, 1, [:integer], :integer) in types
      assert typespec_for(:c_int_identity, 1, [:integer], :integer) in types
      assert typespec_for(:c_long_identity, 1, [:integer], :integer) in types
      assert typespec_for(:isize_identity, 1, [:integer], :integer) in types
      assert typespec_for(:usize_identity, 1, [:integer], :integer) in types
      assert typespec_for(:i32_identity, 1, [:integer], :integer) in types
      assert typespec_for(:i64_identity, 1, [:integer], :integer) in types
    end

    test "for floats" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:f16_identity, 1, [:float], :float) in types
      assert typespec_for(:f32_identity, 1, [:float], :float) in types
      assert typespec_for(:f64_identity, 1, [:float], :float) in types
    end

    test "for bools" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:bool_identity, 1, [:boolean], :boolean) in types
    end

    test "for generic beam types" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:term_identity_a, 1, [:term], :term) in types
      assert typespec_for(:term_identity_b, 1, [:term], :term) in types
    end

    test "for strings" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:c_string_identity, 1, [:binary], :binary) in types
      assert typespec_for(:slice_identity, 1, [:binary], :binary) in types
    end

    test "for int lists" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:i32_slice_identity, 1, [[:integer]], [:integer]) in types
      assert typespec_for(:i64_slice_identity, 1, [[:integer]], [:integer]) in types
    end

    test "for float lists" do
      {:ok, types} = Code.Typespec.fetch_specs(ZiglerTest.Typing)

      assert typespec_for(:f16_slice_identity, 1, [[:float]], [:float]) in types
      assert typespec_for(:f32_slice_identity, 1, [[:float]], [:float]) in types
      assert typespec_for(:f64_slice_identity, 1, [[:float]], [:float]) in types
    end
  end

  defp typespec_for(name, arity, types, return_type) do
    termlist = Enum.map(types, &type_for/1)

    {{name, arity},
    [
      {:type, 4, :fun,
       [
         {:type, 4, :product, termlist},
         type_for(return_type)
       ]}
    ]}
  end

  defp type_for(nil), do: {:atom, 0, nil}
  defp type_for([type]), do: {:type, 0, :list, [type_for(type)]}
  defp type_for(type), do: {:type, 4, type, []}

end
