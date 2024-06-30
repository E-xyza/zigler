defmodule ZiglerTest.Types.SliceTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig,
    otp_app: :zigler,
    leak_check: false,
    nifs: [
      {:slice_u8_test, return: :list},
      {:slice_of_array_u32_list_of_binary, return: {:list, :binary}},
      {:slice_of_array_u32_binary, return: :binary},
      {:slice_of_slice_u32_list_of_binary, return: {:list, :binary}},
      ...
    ]

  ~Z"""
  const beam = @import("beam");

  fn common_slice_fun(passed: anytype) @TypeOf(passed) {
    for (passed) |*value| {value.* += 1;}
    return passed;
  }

  pub fn slice_float_test(passed: []f64) []f64 {
    return common_slice_fun(passed);
  }

  pub fn slice_u8_test(passed: []u8) []u8 {
    return common_slice_fun(passed);
  }

  pub fn slice_string_test(passed: []u8) []u8 {
    return common_slice_fun(passed);
  }

  pub fn const_slice_test(passed: []const u8) usize {
    return passed.len;
  }
  """

  describe "for a generic slice" do
    test "you can pass a list" do
      assert [2.0, 3.0, 4.0] == slice_float_test([1.0, 2.0, 3.0])
    end

    test "you can pass a binary" do
      assert [3.0, 4.0, 5.0] ==
               slice_float_test(<<2.0::float-native, 3.0::float-native, 4.0::float-native>>)
    end

    test "you can work with u8s" do
      assert [2, 3, 4] == slice_u8_test([1, 2, 3])
    end

    test "completely wrong type is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::_ * 64>> | list(float | :infinity | :neg_infinity | :NaN) (for `[]f64`)\n     got: `:bar`\n",
                   fn -> slice_float_test(:bar) end
    end

    test "incorrect value types is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::_ * 64>> | list(float | :infinity | :neg_infinity | :NaN) (for `[]f64`)\n     got: `[\"foo\", :bar, :baz]`\n     at index 0:\n     | expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     | got: `\"foo\"`\n",
                   fn -> slice_float_test(["foo", :bar, :baz]) end
    end

    test "incorrect binary size is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: <<_::_ * 64>> | list(float | :infinity | :neg_infinity | :NaN) (for `[]f64`)\n     got: `<<0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64, 0>>`\n     note: binary size must be a multiple of 8\n     got: 17\n",
                   fn -> slice_float_test(<<1.0::float-native, 2.0::float-native, 0>>) end
    end
  end

  describe "slices of arrays" do
    ~Z"""
    pub fn slice_of_array_u32(slice: [][3]u32) [][3]u32 {
        return slice;
    }
    pub const slice_of_array_u32_list_of_binary = slice_of_array_u32;
    pub const slice_of_array_u32_binary = slice_of_array_u32;
    """

    test "can accept binary" do
      assert [] =
               slice_of_array_u32(
                 <<1::32-native, 2::32-native, 3::32-native, 4::32-native, 5::32-native,
                   6::32-native>>
               )
    end

    test "can accept list of binary" do
      assert [[1, 2, 3], [4, 5, 6]] =
               slice_of_array_u32([
                 <<1::32-native, 2::32-native, 3::32-native>>,
                 <<4::32-native, 5::32-native, 6::32-native>>
               ])
    end

    test "can output as list of binary" do
      assert [
               <<1::32-native, 2::32-native, 3::32-native>>,
               <<4::32-native, 5::32-native, 6::32-native>>
             ] = slice_of_array_u32_list_of_binary([[1, 2, 3], [4, 5, 6]])
    end

    test "can output as binary" do
      assert <<1::32-native, 2::32-native, 3::32-native, 4::32-native, 5::32-native,
               6::32-native>> = slice_of_array_u32_list_of_binary([[1, 2, 3], [4, 5, 6]])
    end
  end

  describe "for u8s strings are" do
    test "available for used as input" do
      assert ~C"bcd" == slice_u8_test("abc")
    end

    test "the default output" do
      assert "bcd" == slice_string_test("abc")
    end
  end

  describe "for const slices" do
    test "it doesn't engage cleanup" do
      assert 3 == const_slice_test("abc")
    end
  end

  describe "for slice of slices" do
    ~Z"""
    pub fn slice_of_slice_u32(slice: [][]u32) [][]u32 {
      return slice;
    }

    pub const slice_of_slice_u32_list_of_binary = slice_of_slice_u32;
    """

    test "list of list input works" do
      assert [[1, 2, 3], [4, 5, 6]] == slice_of_slice_u32([[1, 2, 3], [4, 5, 6]])
      assert [[1, 2], [3, 4, 5]] == slice_of_slice_u32([[1, 2], [3, 4, 5]])
    end

    test "list of binary input works" do
      assert [[1, 2, 3], [4, 5, 6]] == slice_of_slice_u32([<<1::32-native, 2::32-native, 3::32-native>>, <<4::32-native, 5::32-native, 6::32-native>>])
      assert [[1, 2], [3, 4, 5]] == slice_of_slice_u32([<<1::32-native, 2::32-native>>, <<3::32-native, 4::32-native, 5::32-native>>])
    end

    test "binary input only doesn't work" do
      assert_raise ArgumentError, "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(<<_::_ * 32>> | list(integer)) (for `[][]u32`)\n     got: `<<1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0>>`\n", fn ->
        slice_of_slice_u32(<<1::32-native, 2::32-native, 3::32-native, 4::32-native, 5::32-native, 6::32-native>>)
      end
    end

    test "list of binary output works" do
      assert [
               <<1::32-native, 2::32-native, 3::32-native>>,
               <<4::32-native, 5::32-native, 6::32-native>>
             ] = slice_of_slice_u32_list_of_binary([[1, 2, 3], [4, 5, 6]])
    end
  end

  test "slices of structs"
  test "slices of packed structs"
  test "slices of extern structs"

  ~Z"""
  const e = @import("erl_nif");

  pub fn fastlane_beam_term_test(passed: []beam.term) []beam.term {
    for (passed) |*item| {
      const value: f64 = beam.get(f64, item.*, .{}) catch unreachable;
      item.* = beam.make(value + 1.0, .{});
    }
    return passed;
  }

  pub fn fastlane_erl_nif_term_test(passed: []e.ErlNifTerm) []e.ErlNifTerm {
    for (passed) |*item| {
      const value: f64 = beam.get(f64, .{.v = item.*}, .{}) catch unreachable;
      item.* = beam.make(value + 1.0, .{}).v;
    }
    return passed;
  }
  """

  describe "fastlanes for" do
    test "beam.term works" do
      assert [2.0, 3.0, 4.0] = fastlane_beam_term_test([1.0, 2.0, 3.0])
    end

    test "e.ErlNifTerm works" do
      assert [2.0, 3.0, 4.0] = fastlane_erl_nif_term_test([1.0, 2.0, 3.0])
    end
  end

  ~Z"""
  pub fn sentinel_terminated_test(passed: [:0]u8) u8 {
    return passed[3];
  }
  """

  describe "sentinel terminated slices" do
    @tag :skip
    test "are supported" do
      assert 0 == sentinel_terminated_test("foo")
    end
  end
end
