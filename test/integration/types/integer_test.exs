defmodule ZiglerTest.Types.IntegerTest do
  use ExUnit.Case, async: true

  @sizes [7, 8, 32, 48, 64]

  use Zig,
    otp_app: :zigler,
    local_zig: true

  generated_addone_functions =
    Enum.map_join(
      @sizes,
      "\n",
      &"""
      pub fn addone_u#{&1}(x: u#{&1}) u#{&1} { return x + 1; }
      pub fn addone_i#{&1}(x: i#{&1}) i#{&1} { return x + 1; }
      """
    )

  ~z"#{generated_addone_functions}"

  describe "for generated integers" do
    for size <- @sizes do
      ifunction = :"addone_i#{size}"
      ufunction = :"addone_u#{size}"

      test "signed integer of size #{size} works" do
        assert 48 = unquote(ifunction)(47)
      end

      test "unsigned integer of size #{size} works" do
        assert 48 = unquote(ufunction)(47)
      end
    end
  end

  describe "for 64-bit very large integers" do
    test "signed integers it do the right thing" do
      assert -0x7FFF_FFFF_FFFF_FFFF = addone_i64(-0x8000_0000_0000_0000)
    end

    test "unsigned integers it does the right thing" do
      assert 0x8000_0000_0000_0000 = addone_u64(0x7FFF_FFFF_FFFF_FFFF)
    end
  end

  describe "for big integers" do
  end

  describe "when non-integers are passed in" do
  end

  describe "when out of bounds integers are passed in" do
  end
end
