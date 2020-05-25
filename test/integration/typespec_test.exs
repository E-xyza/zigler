defmodule ZiglerTest.Integration.TypespecTest do
  use ExUnit.Case, async: true

  #
  # note that this module doesn't make sense unless you have the context of the
  # support module `ZiglerTest.Types`.  This support module can be found in the
  # following location:
  #
  # test/support/types.ex
  #

  @moduletag :typespec

  {:ok, type_list} = Code.Typespec.fetch_specs(ZiglerTest.Types)
  @types type_list

  # we need a consistent way of matching things in the type list
  defp assert_typespec(fn_name, arity, args, return) do
    assert {_, [spec]} = Enum.find(@types, &match?({{^fn_name, ^arity}, _}, &1))
    assert {:type, _, :fun, [{:type, _, :product, ts_args}, ts_return]} = spec
    args
    |> Enum.zip(ts_args)
    |> Enum.each(&compare_type/1)
    compare_type(return, ts_return)
  end

  defp compare_type({type, target}), do: compare_type(type, target)
  defp compare_type(type, target) when is_atom(type), do: assert match?({:type, _, ^type, _}, target)
  defp compare_type({:atom, atom}, target), do: assert match?({:atom, _, ^atom}, target)
  defp compare_type({:list, type}, target) do
    assert match?({:type, _, :list, [{:type, _, ^type, _}]}, target)
  end
  defp compare_type(first..last, target) when first >= 0 do
    assert match?({:type, _, :range, [{:integer, _, ^first}, {:integer, _, ^last}]}, target)
  end
  defp compare_type(first..last, target) do
    neg_first = -first
    assert match?({:type, _, :range, [
      {:op, _, :-, {:integer, _, ^neg_first}},
      {:integer, _, ^last}]}, target)
  end

  describe "the dummy elixir typespec matches" do
    test "for basic integer" do
      assert_typespec(:dummy_integer, 1, [:integer], :integer)
    end
  end

  describe "for the selected function values" do
    test "void return gives nil" do
      assert_typespec(:void_out, 0, [], {:atom, :nil})
    end

    test "u32 is specced correctly" do
      assert_typespec(:u32_in_out, 1, [0..0xFFFF_FFFF], 0..0xFFFF_FFFF)
    end

    @i32_range -2_147_483_648..2_147_483_647
    test "i32 is specced correctly" do
      assert_typespec(:i32_in_out, 1, [@i32_range], @i32_range)
    end

    test "i64 is specced correctly" do
      assert_typespec(:i64_in_out, 1, [:integer], :integer)
    end

    test "f64 is specced correctly" do
      assert_typespec(:f64_in_out, 1, [:float], :float)
    end

    test "bool is specced correctly" do
      assert_typespec(:bool_in_out, 1, [:boolean], :boolean)
    end

    test "term is specced correctly" do
      assert_typespec(:term_in_out, 1, [:term], :term)
      assert_typespec(:eterm_in_out, 1, [:term], :term)
    end

    test "pid is specced correctly" do
      assert_typespec(:pid_in_out, 1, [:pid], :pid)
      assert_typespec(:epid_in_out, 1, [:pid], :pid)
    end

    test "atom is specced correctly" do
      assert_typespec(:atom_in_out, 1, [:atom], :atom)
    end

    test "string is specced correctly" do
      assert_typespec(:str_in_out, 1, [:binary], :binary)
    end

    test "slice is specced correctly" do
      assert_typespec(:islice_in_out, 1, [{:list, :integer}], {:list, :integer})
    end

    test "multiargument is specced correctly" do
      assert_typespec(:multiarg, 2, [:integer, :float], :float)
    end

    test "with env it's specced correctly" do
      assert_typespec(:env_zero, 0, [], :integer)
      assert_typespec(:eenv_zero, 0, [], :integer)

      assert_typespec(:env_one, 1, [:integer], :integer)
      assert_typespec(:eenv_one, 1, [:integer], :integer)
    end
  end
end
