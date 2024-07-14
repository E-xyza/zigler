defmodule ZiglerTest.Types.EnumTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  pub const EnumType = enum{ foo, bar };

  pub fn untagged_swap(value: EnumType) EnumType {
    return if (value == .foo) .bar else .foo;
  }
  """

  describe "given an enum" do
    test "you can pass in atoms to get the value out" do
      assert :foo = untagged_swap(:bar)
      assert :bar = untagged_swap(:foo)
    end

    test "you can pass in integers and they'll be coerced" do
      assert :foo = untagged_swap(1)
      assert :bar = untagged_swap(0)
    end

    test "if you try to use something that isn't an atom or integer" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: 0 | 1 | :foo | :bar (for `EnumType`)\n     got: `\"foo\"`\n",
                   fn -> untagged_swap("foo") end
    end

    test "if you try to use an invalid atom" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     note: not an atom value for EnumType (should be one of `[:foo, :bar]`)\n     expected: 0 | 1 | :foo | :bar (for `EnumType`)\n     got: `:zag`\n",
                   fn -> untagged_swap(:zag) end
    end

    test "if you try to use an invalid number" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     note: not an integer value for EnumType (should be one of `[0, 1]`)\n     expected: integer (for `u1`)\n     got: `42`\n     note: out of bounds (0..1)\n     expected: 0 | 1 | :foo | :bar (for `EnumType`)\n     got: `42`\n",
                   fn -> untagged_swap(42) end
    end
  end

  describe "if you try to make" do
    test "zero item enum, it's a compiler error" do
      assert_raise CompileError,
                   "zigler encountered the unusable type .Elixir.ZiglerTest.Types.ZeroItemEnum.E",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.Types.ZeroItemEnum do
                           use Zig, otp_app: :zigler, dir: unquote(__DIR__)

                           ~Z"""
                           const E = enum{ };
                           pub fn foo(e: E) E {
                             return e;
                           }
                           """
                         end
                       end
                     )
                   end
    end

    test "one item enum, it's a compiler error" do
      assert_raise CompileError,
                   "zigler encountered the unusable type .Elixir.ZiglerTest.Types.OneItemEnum.E",
                   fn ->
                     Code.compile_quoted(
                       quote do
                         defmodule ZiglerTest.Types.OneItemEnum do
                           use Zig, otp_app: :zigler, dir: unquote(__DIR__)

                           ~Z"""
                           const E = enum{ ok };
                           pub fn foo(e: E) E {
                             return e;
                           }
                           """
                         end
                       end
                     )
                   end
    end
  end
end
