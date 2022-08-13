defmodule ZiglerTest.Unit.Type.IntegerTest do
  use ExUnit.Case, async: true

  alias Zig.Type
  alias Zig.Type.Integer

  import Type, only: :macros

  describe "signed integers" do
    test "parse correctly" do
      assert %Integer{signedness: :signed, bits: 8} = Type.parse("i8")
      assert %Integer{signedness: :signed, bits: 15} = Type.parse("i15")
    end

    test "can be generated using sigil t" do
      assert %Integer{signedness: :signed, bits: 8} = ~t"i8"
      assert %Integer{signedness: :signed, bits: 15} =  ~t"i15"
    end

    test "inspect as the expected sigil t" do
      assert "~t(i8)" == inspect(~t(i8))
      assert "~t(i15)" == inspect(~t(i15))
    end

    test "to_string as themeslves" do
      assert "i8" == to_string(~t(i8))
      assert "i15" == to_string(~t(i15))
    end
  end

  describe "unsigned integers" do
    test "parse correctly" do
      assert %Integer{signedness: :unsigned, bits: 8} = Type.parse("u8")
      assert %Integer{signedness: :unsigned, bits: 15} = Type.parse("u15")
    end

    test "can be generated using sigil t" do
      assert %Integer{signedness: :unsigned, bits: 8} = ~t"u8"
      assert %Integer{signedness: :unsigned, bits: 15} =  ~t"u15"
    end

    test "inspect as the expected sigil t" do
      assert "~t(u8)" == inspect(~t(u8))
      assert "~t(u15)" == inspect(~t(u15))
    end

    test "to_string as themeslves" do
      assert "u8" == to_string(~t(u8))
      assert "u15" == to_string(~t(u15))
    end
  end
end
