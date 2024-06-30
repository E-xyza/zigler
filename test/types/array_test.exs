defmodule ZiglerTest.Types.ArrayTest do
  use ZiglerTest.IntegrationCase, async: true

  @tag :skip
  test "reactivate leak check below"

  use Zig,
    otp_app: :zigler,
    # leak_check: true,
    nifs: [
      {:array_float_binary_test, return: :binary},
      {:array_u8_test, return: :list},
      {:array_of_u8_array_as_list, return: {:list, :list}},
      {:array_of_u32_array_as_binary, return: :binary},
      {:array_of_u32_array_as_list_binary, return: {:list, :binary}},
      {:array_of_packed_structs_list_of_map, return: {:list, :map}},
      {:array_of_packed_structs_binary, return: :binary},
      {:array_of_extern_structs_list_of_binary, return: {:list, :binary}},
      {:array_of_extern_structs_binary, return: :binary},
      {:mut_array_float_test, return: :list},
      {:mut_array_u8_test, return: :list},
      {:fastlane_beam_term_ptr_test, return: :noclean},
      {:fastlane_erl_nif_term_ptr_test, return: :noclean},
      ...
    ]

  ## BASIC ARRAYS

  ~Z"""
  fn common_array_fun(passed: anytype) @TypeOf(passed) {
    var returned : @TypeOf(passed) = undefined;

    for (passed, 0..) |value, index| {
      returned[index] = value + 1;
    }

    return returned;
  }

  pub fn array_float_test(passed: [3]f64) [3]f64 {
    return common_array_fun(passed);
  }

  pub fn array_float_binary_test(passed: [3]f64) [3]f64 {
    return common_array_fun(passed);
  }

  pub fn array_u8_test(passed: [3]u8) [3]u8 {
    return common_array_fun(passed);
  }

  pub fn array_of_u8_array_as_list(passed: [3][3]u8) [3][3]u8 {
    return passed;
  }

  pub fn array_of_u32_array_as_binary(passed: [3][3]u32) [3][3]u32 {
    return passed;
  }

  pub fn array_of_u32_array_as_list_binary(passed: [3][3]u32) [3][3]u32 {
    return passed;
  }

  pub fn array_string_test(passed: [3]u8) [3]u8 {
    return common_array_fun(passed);
  }
  """

  describe "for a generic array" do
    test "you can pass a list" do
      assert [2.0, 3.0, 4.0] == array_float_test([1.0, 2.0, 3.0])
    end

    test "you can pass a binary" do
      assert [2.0, 3.0, 4.0] ==
               array_float_test(<<1.0::float-native, 2.0::float-native, 3.0::float-native>>)
    end

    test "you can get back a binary" do
      assert <<2.0::float-native, 3.0::float-native, 4.0::float-native>> ==
               array_float_binary_test([1.0, 2.0, 3.0])
    end

    test "you can work with u8s" do
      assert [2, 3, 4] == array_u8_test([1, 2, 3])
    end

    test "completely wrong type is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::192>> (for `[3]f64`)\n     got: `:bar`\n",
                   fn -> array_float_test(:bar) end
    end

    test "too few elements is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::192>> (for `[3]f64`)\n     got: `[1.0, 2.0]`\n     note: length 3 expected but got length 2\n",
                   fn -> array_float_test([1.0, 2.0]) end
    end

    test "too many elements is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::192>> (for `[3]f64`)\n     got: `[1.0, 2.0, 3.0, 4.0]`\n     note: length 3 expected but got length 4\n",
                   fn -> array_float_test([1.0, 2.0, 3.0, 4.0]) end
    end

    test "incorrect value types is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::192>> (for `[3]f64`)\n     got: `[\"foo\", :bar, :baz]`\n     at index 0:\n     | expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     | got: `\"foo\"`\n",
                   fn -> array_float_test(["foo", :bar, :baz]) end
    end

    test "incorrect binary size is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::192>> (for `[3]f64`)\n     got: `<<0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64>>`\n     note: binary size 24 expected but got size 16\n",
                   fn -> array_float_test(<<1.0::float-native, 2.0::float-native>>) end
    end
  end

  describe "for u8s strings are" do
    test "available for used as input" do
      assert ~C"bcd" == array_u8_test("abc")
    end

    test "the default output" do
      assert "bcd" == array_string_test("abc")
    end

    test "not tolerated with wrong length" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(integer) | <<_::24>> (for `[3]u8`)\n     got: `\"fo\"`\n     note: binary size 3 expected but got size 2\n",
                   fn -> array_string_test("fo") end
    end
  end

  describe "for nested arrays" do
    test "you can set u8 to be internally list" do
      assert [[1, 2, 3], [4, 5, 6], [7, 8, 9]] =
               array_of_u8_array_as_list(<<1, 2, 3, 4, 5, 6, 7, 8, 9>>)
    end

    test "you can set u32 to be generally binary" do
      assert <<1::32-native, 2::32-native, 3::32-native, 4::32-native, 5::32-native, 6::32-native,
               7::32-native, 8::32-native,
               9::32-native>> =
               array_of_u32_array_as_binary([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    end

    test "you can set internal u32 to be binary" do
      assert [
               <<1::32-native, 2::32-native, 3::32-native>>,
               <<4::32-native, 5::32-native, 6::32-native>>,
               <<7::32-native, 8::32-native, 9::32-native>>
             ] = array_of_u32_array_as_list_binary([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    end
  end

  describe "for an array of structs" do
    ~Z"""
    const S = struct { val: u32 };
    pub fn array_of_structs(a: [3]S) [3]S { return a; }

    const P = packed struct {val: u32};
    const E = extern struct {val: u32};

    pub fn array_of_packed_structs(a: [3]P) [3]P { return a; }

    pub const array_of_packed_structs_list_of_map = array_of_packed_structs;
    pub const array_of_packed_structs_binary = array_of_packed_structs;

    pub fn array_of_extern_structs(a: [3]E) [3]E { return a; }

    pub const array_of_extern_structs_list_of_binary = array_of_extern_structs;
    pub const array_of_extern_structs_binary = array_of_extern_structs;
    """

    test "basic structs works" do
      assert [%{val: 1}, %{val: 2}, %{val: 3}] ==
               array_of_structs([%{val: 1}, %{val: 2}, %{val: 3}])
    end

    test "basic structs can't be made from binaries" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(map | keyword) (for `[3]S`)\n     got: `<<1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0>>`\n",
                   fn ->
                     array_of_structs(<<1::32-native, 2::32-native, 3::32-native>>)
                   end
    end

    test "packed structs default to returning as binaries" do
      assert [<<1::32-native>>, <<2::32-native>>, <<3::32-native>>] ==
               array_of_packed_structs([%{val: 1}, %{val: 2}, %{val: 3}])
    end

    test "packed structs can be forced to output as map" do
      assert [%{val: 1}, %{val: 2}, %{val: 3}] ==
               array_of_packed_structs_list_of_map([%{val: 1}, %{val: 2}, %{val: 3}])
    end

    test "packed structs can be forced to output as a single binary" do
      assert <<1::32-native, 2::32-native, 3::32-native>> ==
               array_of_packed_structs_binary([%{val: 1}, %{val: 2}, %{val: 3}])
    end

    test "packed structs can be made from binaries" do
      assert [<<1::32-native>>, <<2::32-native>>, <<3::32-native>>] ==
               array_of_packed_structs([<<1::32-native>>, <<2::32-native>>, <<3::32-native>>])
    end

    test "packed structs can be made from a single binary" do
      assert [<<1::32-native>>, <<2::32-native>>, <<3::32-native>>] ==
               array_of_packed_structs(<<1::32-native, 2::32-native, 3::32-native>>)
    end

    test "correct error message when element binary is of the wrong size" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(map | keyword | <<_::32>>) | <<_::96>> (for `[3]P`)\n     got: `[<<1, 0, 0, 0>>, <<2, 0, 0, 0>>, <<0, 0, 0>>]`\n     at index 2:\n     | expected: map | keyword | <<_::32>> (for `P`)\n     | got: `<<0, 0, 0>>`\n     | note: binary size 4 expected but got size 3\n",
                   fn ->
                     array_of_packed_structs([<<1::32-native>>, <<2::32-native>>, <<0, 0, 0>>])
                   end
    end

    test "correct error message when full binary is of the wrong size" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(map | keyword | <<_::32>>) | <<_::96>> (for `[3]P`)\n     got: `<<1, 0, 0, 0, 2, 0, 0, 0>>`\n     note: binary size 12 expected but got size 8\n",
                   fn ->
                     array_of_packed_structs(<<1::32-native, 2::32-native>>)
                   end
    end

    test "extern structs default to output as a map" do
      assert [%{val: 1}, %{val: 2}, %{val: 3}] ==
               array_of_extern_structs([%{val: 1}, %{val: 2}, %{val: 3}])
    end

    test "extern structs can be forced to output as binary" do
      assert [<<1::32-native>>, <<2::32-native>>, <<3::32-native>>] ==
               array_of_extern_structs_list_of_binary([%{val: 1}, %{val: 2}, %{val: 3}])
    end

    test "extern structs can be forced to output as a single binary" do
      assert <<1::32-native, 2::32-native, 3::32-native>> ==
               array_of_extern_structs_binary([%{val: 1}, %{val: 2}, %{val: 3}])
    end

    test "extern structs can be made from binaries" do
      assert [%{val: 1}, %{val: 2}, %{val: 3}] ==
               array_of_extern_structs([<<1::32-native>>, <<2::32-native>>, <<3::32-native>>])
    end

    test "extern structs can be made from a single binary" do
      assert [%{val: 1}, %{val: 2}, %{val: 3}] ==
               array_of_extern_structs(<<1::32-native, 2::32-native, 3::32-native>>)
    end

    test "correct error message for extern structs when element binary is of the wrong size" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(map | keyword | <<_::32>>) | <<_::96>> (for `[3]E`)\n     got: `[<<1, 0, 0, 0>>, <<2, 0, 0, 0>>, <<0, 0, 0>>]`\n     at index 2:\n     | expected: map | keyword | <<_::32>> (for `E`)\n     | got: `<<0, 0, 0>>`\n     | note: binary size 4 expected but got size 3\n",
                   fn ->
                     array_of_extern_structs([<<1::32-native>>, <<2::32-native>>, <<0, 0, 0>>])
                   end
    end

    test "correct error message for extern structs when full binary is of the wrong size" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(map | keyword | <<_::32>>) | <<_::96>> (for `[3]E`)\n     got: `<<1, 0, 0, 0, 2, 0, 0, 0>>`\n     note: binary size 12 expected but got size 8\n",
                   fn ->
                     array_of_extern_structs(<<1::32-native, 2::32-native>>)
                   end
    end
  end

  ## "PSEUDO-MUTABLE" ARRAYS.  Note that "responsibility" for the
  # array data is with the calling function.

  ~Z"""
  fn common_array_mut(passed: anytype) void {
    for (passed) |*value| { value.* += 1; }
  }

  pub fn mut_array_float_test(passed: *[3]f64) *[3]f64 {
    common_array_mut(passed);
    return passed;
  }

  pub fn mut_array_u8_test(passed: *[3]u8) *[3]u8 {
    common_array_mut(passed);
    return passed;
  }

  pub fn mut_array_string_test(passed: *[3]u8) *[3]u8 {
    common_array_mut(passed);
    return passed;
  }
  """

  describe "for a generic mutable array" do
    test "you can do a pseudo-mutable pass" do
      assert [2.0, 3.0, 4.0] == mut_array_float_test([1.0, 2.0, 3.0])
      assert [2, 3, 4] == mut_array_u8_test([1, 2, 3])
    end

    test "you can pass a string for a u8 array" do
      assert ~C"bcd" == mut_array_u8_test("abc")
    end

    test "returning a string is default" do
      assert "bcd" == mut_array_string_test("abc")
    end
  end

  ~Z"""
  pub fn sentinel_terminated_test(passed: [3:0]u8) u8 {
    return passed[3];
  }
  """

  describe "sentinel terminated arrays" do
    test "are supported" do
      assert 0 == sentinel_terminated_test("foo")
    end
  end

  ~Z"""
  const beam = @import("beam");
  const e = @import("erl_nif");

  pub fn fastlane_beam_term_test(passed: [3]beam.term) [3]beam.term {
    var result: [3]beam.term = undefined;
    for (&result, 0..) |*item, index| {
      const value: f64 = beam.get(f64, passed[index], .{}) catch unreachable;
      item.* = beam.make(value + 1.0, .{});
    }
    return result;
  }

  pub fn fastlane_erl_nif_term_test(passed: [3]e.ErlNifTerm) [3]e.ErlNifTerm {
    var result: [3]e.ErlNifTerm = undefined;
    for (&result, 0..) |*item, index| {
      const value: f64 = beam.get(f64, .{.v = passed[index]}, .{}) catch unreachable;
      item.* = beam.make(value + 1.0, .{}).v;
    }
    return result;
  }

  pub fn fastlane_beam_term_ptr_test(passed: *[3]beam.term) *[3]beam.term {
    for (passed) |*item| {
      const value: f64 = beam.get(f64, item.*, .{}) catch unreachable;
      item.* = beam.make(value + 1.0, .{});
    }
    return passed;
  }

  pub fn fastlane_erl_nif_term_ptr_test(passed: *[3]e.ErlNifTerm) *[3]e.ErlNifTerm {
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

    test "beam.term pointer works" do
      assert [2.0, 3.0, 4.0] = fastlane_beam_term_ptr_test([1.0, 2.0, 3.0])
    end

    test "e.ErlNifTerm pointer works" do
      assert [2.0, 3.0, 4.0] = fastlane_erl_nif_term_ptr_test([1.0, 2.0, 3.0])
    end
  end
end
