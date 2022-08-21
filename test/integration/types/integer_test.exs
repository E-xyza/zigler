defmodule ZiglerTest.Types.IntegerTest do
  use ExUnit.Case, async: true

  @sizes [7, 8, 32, 48, 64]

  use Zig,
    otp_app: :zigler,
    local_zig: true,
    dump: true,
    nifs: [
      :addone_u7,
      :addone_i7,
      :addone_u8,
      :addone_i8,
      :addone_u32,
      :addone_i32,
      :addone_u48,
      :addone_i48,
      :addone_u64,
      :addone_i64
    ]

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

  describe "for 64-bit unsigned very large integers" do
    test "it does the right thing" do
      assert 0x8000_0000_0000_0001 = addone_u64(0x8000_0000_0000_0000)
    end
  end

  describe "for big integers" do
  end

  describe "when non-integers are passed in" do
  end

  describe "when out of bounds integers are passed in" do
  end
end
