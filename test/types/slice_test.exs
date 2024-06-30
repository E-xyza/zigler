defmodule ZiglerTest.Types.SliceTest do
  use ZiglerTest.IntegrationCase, async: true

  @tag :skip
  test "remove leak check below"

  use Zig,
    otp_app: :zigler,
    leak_check: false,
    nifs: [
      {:slice_u8_test, return: :list},
      {:slice_of_array_u32_list_of_binary, return: {:list, :binary}},
      {:slice_of_array_u32_binary, return: :binary},
      {:slice_of_slice_u32_list_of_binary, return: {:list, :binary}},
      {:slice_of_structs_list, return: {:list, {:map, val: :list}}},
      {:slice_of_packed_structs_maps, return: {:list, :map}},
      {:slice_of_packed_structs_binary, return: :binary},
      {:slice_of_extern_structs_list_of_binary, return: {:list, :binary}},
      {:slice_of_extern_structs_binary, return: :binary},
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
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::_*64>> (for `[]f64`)\n     got: `:bar`\n",
                   fn -> slice_float_test(:bar) end
    end

    test "incorrect value types is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::_*64>> (for `[]f64`)\n     got: `[\"foo\", :bar, :baz]`\n     at index 0:\n     | expected: float | :infinity | :neg_infinity | :NaN (for `f64`)\n     | got: `\"foo\"`\n",
                   fn -> slice_float_test(["foo", :bar, :baz]) end
    end

    test "incorrect binary size is not tolerated" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(float | :infinity | :neg_infinity | :NaN) | <<_::_*64>> (for `[]f64`)\n     got: `<<0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 0, 64, 0>>`\n     note: binary size must be a multiple of 8\n     got: 17\n",
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
      assert [[1, 2, 3], [4, 5, 6]] =
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

    test "gives correct argumenterror if an internal binary is incorrect" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(list(integer) | <<_::96>>) | <<_::_*96>> (for `[][3]u32`)\n     got: `[<<1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0>>, <<4, 0, 0, 0, 5, 0, 0, 0>>]`\n     at index 1:\n     | expected: list(integer) | <<_::96>> (for `[3]u32`)\n     | got: `<<4, 0, 0, 0, 5, 0, 0, 0>>`\n     | note: binary size 12 expected but got size 8\n",
                   fn ->
                     slice_of_array_u32([
                       <<1::32-native, 2::32-native, 3::32-native>>,
                       <<4::32-native, 5::32-native>>
                     ])
                   end
    end

    test "gives correct argumenterror if the full binary is incorrect" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(list(integer) | <<_::96>>) | <<_::_*96>> (for `[][3]u32`)\n     got: `<<1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0>>`\n     note: binary size must be a multiple of 12\n     got: 20\n",
                   fn ->
                     slice_of_array_u32(
                       <<1::32-native, 2::32-native, 3::32-native, 4::32-native, 5::32-native>>
                     )
                   end
    end

    test "can output as list of binary" do
      assert [
               <<1::32-native, 2::32-native, 3::32-native>>,
               <<4::32-native, 5::32-native, 6::32-native>>
             ] = slice_of_array_u32_list_of_binary([[1, 2, 3], [4, 5, 6]])
    end

    test "can output as binary" do
      assert <<1::32-native, 2::32-native, 3::32-native, 4::32-native, 5::32-native,
               6::32-native>> = slice_of_array_u32_binary([[1, 2, 3], [4, 5, 6]])
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
      assert [[1, 2, 3], [4, 5, 6]] ==
               slice_of_slice_u32([
                 <<1::32-native, 2::32-native, 3::32-native>>,
                 <<4::32-native, 5::32-native, 6::32-native>>
               ])

      assert [[1, 2], [3, 4, 5]] ==
               slice_of_slice_u32([
                 <<1::32-native, 2::32-native>>,
                 <<3::32-native, 4::32-native, 5::32-native>>
               ])
    end

    test "binary input only doesn't work" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(list(integer) | <<_::_*32>>) (for `[][]u32`)\n     got: `<<1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0>>`\n",
                   fn ->
                     slice_of_slice_u32(
                       <<1::32-native, 2::32-native, 3::32-native, 4::32-native, 5::32-native,
                         6::32-native>>
                     )
                   end
    end

    test "list of binary output works" do
      assert [
               <<1::32-native, 2::32-native, 3::32-native>>,
               <<4::32-native, 5::32-native, 6::32-native>>
             ] = slice_of_slice_u32_list_of_binary([[1, 2, 3], [4, 5, 6]])
    end
  end

  describe "slices of structs" do
    ~Z"""
    const S = struct { val: []u8 };
    pub fn slice_of_structs(slice: []S) []S { return slice; }
    pub const slice_of_structs_list = slice_of_structs;
    """

    test "works" do
      assert [%{val: "foo"}, %{val: "bar"}] == slice_of_structs([%{val: "foo"}, %{val: "bar"}])
    end

    test "can have do lists internally" do
      assert [%{val: ~C'foo'}, %{val: ~C'bar'}] ==
               slice_of_structs_list([%{val: "foo"}, %{val: "bar"}])
    end
  end

  describe "slices of packed structs" do
    ~Z"""
    const P = packed struct { val: u32 };
    pub fn slice_of_packed_structs(slice: []P) []P { return slice; }
    pub const slice_of_packed_structs_maps = slice_of_packed_structs;
    pub const slice_of_packed_structs_binary = slice_of_packed_structs;
    """

    test "defaults to list of binary" do
      assert [<<47::32-native>>, <<47::32-native>>] ==
               slice_of_packed_structs([%{val: 47}, %{val: 47}])
    end

    test "can accept list of binary" do
      assert [<<47::32-native>>, <<47::32-native>>] ==
               slice_of_packed_structs([<<47::32-native>>, <<47::32-native>>])
    end

    test "can accept binary" do
      assert [<<47::32-native>>, <<47::32-native>>] ==
               slice_of_packed_structs(<<47::32-native, 47::32-native>>)
    end

    test "can be forced to output maps" do
      assert [%{val: 47}, %{val: 47}] == slice_of_packed_structs_maps([%{val: 47}, %{val: 47}])
    end

    test "can be forced to output full binary" do
      assert <<47::32-native, 47::32-native>> ==
               slice_of_packed_structs_binary([%{val: 47}, %{val: 47}])
    end

    test "argumenterror on incorrect binary size" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(map | keyword | <<_::32>>) | <<_::_*32>> (for `[]P`)\n     got: `<<0, 0, 0>>`\n     note: binary size must be a multiple of 4\n     got: 3\n",
                   fn ->
                     slice_of_packed_structs(<<0, 0, 0>>)
                   end
    end
  end

  describe "slices of extern structs" do
    ~Z"""
    const E = extern struct { val: u32 };
    pub fn slice_of_extern_structs(slice: []E) []E { return slice; }
    pub const slice_of_extern_structs_list_of_binary = slice_of_extern_structs;
    pub const slice_of_extern_structs_binary = slice_of_extern_structs;
    """

    test "defaults to map" do
      assert [%{val: 47}, %{val: 47}] == slice_of_extern_structs([%{val: 47}, %{val: 47}])
    end

    test "can accept list of binary" do
      assert [%{val: 47}, %{val: 47}] ==
               slice_of_extern_structs([<<47::32-native>>, <<47::32-native>>])
    end

    test "can accept binary" do
      assert [%{val: 47}, %{val: 47}] == slice_of_extern_structs(<<47::32-native, 47::32-native>>)
    end

    test "can be forced to output binaries" do
      assert [<<47::32-native>>, <<47::32-native>>] ==
               slice_of_extern_structs_list_of_binary([%{val: 47}, %{val: 47}])
    end

    test "can be forced to output full binary" do
      assert <<47::32-native, 47::32-native>> ==
               slice_of_extern_structs_binary([%{val: 47}, %{val: 47}])
    end

    test "raises argumenterror on incorrect size" do
      assert_raise ArgumentError,
                   "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: list(map | keyword | <<_::32>>) | <<_::_*32>> (for `[]E`)\n     got: `<<0, 0, 0>>`\n     note: binary size must be a multiple of 4\n     got: 3\n",
                   fn ->
                     slice_of_extern_structs(<<0, 0, 0>>)
                   end
    end
  end

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
