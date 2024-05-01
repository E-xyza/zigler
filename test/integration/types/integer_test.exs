defmodule ZiglerTest.Types.IntegerTest do
  use ZiglerTest.IntegrationCase, async: true

  @sizes [7, 8, 32, 48, 64]

  use Zig, otp_app: :zigler, dump: true

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

      test "non-integer for size #{size} fails" do
        size = unquote(size)

        assert_raise ArgumentError,
                     "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: integer (for `i#{size}`)\n     got: `\"foo\"`\n",
                     fn ->
                       unquote(ifunction)("foo")
                     end
      end

      test "out of bounds unsigned integer of size #{size} fails" do
        size = unquote(size)
        import Bitwise
        limit = 1 <<< size

        assert_raise ArgumentError,
                     "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: integer (for `u#{size}`)\n     got: `#{limit}`\n     note: out of bounds (0..#{limit - 1})\n",
                     fn ->
                       unquote(ufunction)(limit)
                     end
      end

      test "unsigned integer of size #{size} works" do
        assert 48 = unquote(ufunction)(47)
      end
    end
  end

  describe "for zero bit integers" do
    ~Z"""
    pub fn zerobit(v: u0) u0 { return v; }
    """

    test "it works, but is kind of useless" do
      assert 0 = zerobit(0)
    end

    test "non-integer fails for size 0" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: zero (for `u0`)\n     got: `\"foo\"`\n",
                   fn -> zerobit("foo") end
    end

    test "out of bounds integer of size 0 fails" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: zero (for `u0`)\n     got: `1`\n",
                   fn -> zerobit(1) end
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

  describe "for super large integers" do
    ~Z"""
    pub fn test_u128(x: u128) u128 { return (x << 4) + x - 0x0101_0101_0101_0101_0101_0101_0101; }
    // because we can.
    pub fn test_u129(x: u129) u129 { return (x << 4) + x - 0x0101_0101_0101_0101_0101_0101_0101; }
    pub fn test_u256(x: u256) u256 { return (x << 4) + x - 0x0101_0101_0101_0101_0101_0101_0101; }
    """

    # NB these numbers and transformations were selected to debug endianness issues.
    test "zigler can marshal in and out correctly" do
      assert 0x1021_3243_5465_7687_98A9_BACB_DCED = test_u128(0x102_0304_0506_0708_090A_0B0C_0D0E)
      assert 0x1021_3243_5465_7687_98A9_BACB_DCED = test_u129(0x102_0304_0506_0708_090A_0B0C_0D0E)
      assert 0x1021_3243_5465_7687_98A9_BACB_DCED = test_u256(0x102_0304_0506_0708_090A_0B0C_0D0E)
    end

    ~Z"""
    pub fn test_u65(x: u65) u65 { return x; }
    pub fn test_i65(x: i65) i65 { return x; }
    """

    test "type checking on large integer" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: integer (for `u65`)\n     got: `\"foo\"`\n",
                   fn ->
                     test_u65("foo")
                   end
    end

    test "bounds checking on unsigned large integer" do
      limit = Bitwise.<<<(1, 65)

      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: integer (for `u65`)\n     got: `#{limit}`\n     note: out of bounds (0..#{limit - 1})\n",
                   fn ->
                     test_u65(limit)
                   end
    end

    test "bounds checking on signed large integer" do
      limit = Bitwise.<<<(1, 64)
      value = -limit - 1

      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: integer (for `i65`)\n     got: `#{value}`\n     note: out of bounds (#{-limit}..#{limit - 1})\n",
                   fn ->
                     test_i65(value)
                   end
    end
  end

  describe "for c integer types" do
    @ctypes ~w(c_short c_ushort c_int c_uint c_long c_longlong c_ulonglong)
    # note that c_ulong is missing, it's not entirely clear why that doesn't work.

    for type <- @ctypes do
      function = :"addone_#{type}"

      ~z"""
      pub fn #{function}(value: #{type}) #{type} { return value + 1; }
      """

      test "c integer #{type} does the right thing" do
        assert 48 = unquote(function)(47)
      end
    end
  end

  describe "for big integers" do
    @tag :skip
    test "can be marshalled into zig bigint type"
  end
end
