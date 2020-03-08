defmodule ZiglerTest.DryRunTypespecTest do
  use ExUnit.Case, async: true

  @moduletag :typespec

  {:ok, type_list} = Code.Typespec.fetch_specs(ZiglerTest.Types)
  @types type_list

  # we need a consistent way of matching things in the type list
  defp assert_typespec(fn_name, arity, args, return) do
    assert {_, [spec]} = Enum.find(@types, &match?({{^fn_name, ^arity}, _}, &1))
    assert {:type, _, :fun, [{:type, _, :product, ts_args}, ts_return]} = spec
    ts_args
    |> Enum.zip(args)
    |> Enum.each(fn {ts_arg, arg} -> assert match?({:type, _, ^arg, _}, ts_arg) end)
    case return do
      type when is_atom(type) ->
        assert match?({:type, _, ^type, _}, ts_return)
      {:atom, atom} ->
        assert match?({:atom, _, ^atom}, ts_return)
    end
  end

  describe "the dummy elixir typespec matches" do
    test "for basic integer" do
      assert_typespec(:dummy_integer, 1, [:integer], :integer)
    end
  end

  describe "for the egress values" do
    test "void return gives nil" do
      assert_typespec(:void_out, 0, [], {:atom, :nil})
    end
  end
end
