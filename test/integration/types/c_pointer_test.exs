defmodule ZiglerTest.Types.CPointerTest do
  use ExUnit.Case, async: true

  use Zig, otp_app: :zigler,
    nifs: [
      :cpointer_test,
      :cpointer_list_test,
      :cpointer_struct_list_test,
      :cpointer_u8_return_test,
      {:cpointer_u8_list_return_test, return: :list},
      :cpointer_struct_return_test,
      :cpointer_struct_list_return_test,
      :cpointer_null_return_test,
    ]

  ## C pointers as single pointers
  ## C pointers can be a single pointer to a struct, which makes it a "mutable" struct.
  ~Z"""
  pub const TestStruct = extern struct { value: i32 };

  pub fn cpointer_test(passed: [*c]TestStruct) ?i32 {
    if (passed) |unwrapped| {
      return unwrapped.*.value;
    } else {
      return null;
    }
  }
  """

  describe "for a struct cpointer" do
    test "you can pass a map" do
      assert 47 = cpointer_test(%{value: 47})
    end

    test "you can pass null" do
      assert is_nil(cpointer_test(nil))
    end

    test "you can't pass a keyword list" do
      assert_raise ArgumentError, "", fn ->
        cpointer_test(value: 47)
      end
    end
  end

  ~Z"""
  pub fn cpointer_list_test(list: [*c]u8) ?u32 {
    var sum: u32 = 0;
    if (list) |_| {
      for (list[0..3]) |item| {sum += item;}
      return sum;
    } else return null;
  }

  pub fn cpointer_struct_list_test(list: [*c]TestStruct) ?i32 {
    var sum: i32 = 0;
    for (list[0..3]) |item| {sum += item.value;}
    return sum;
  }
  """

  describe "for a list cpointer" do
    test "you can pass a list" do
      assert 6 == cpointer_list_test([1, 2, 3])
    end

    test "you can pass null" do
      assert is_nil(cpointer_list_test(nil))
    end

    test "you can pass a string for u8" do
      assert Enum.sum(~C"abc") == cpointer_list_test("abc")
    end

    test "you can pass a list of structs" do
      assert 6 = cpointer_struct_list_test([%{value: 1}, %{value: 2}, %{value: 3}])
    end
  end

  ~Z"""
  var u8_array_list = [_]u8{'a', 'b', 'c', 0};
  pub fn cpointer_u8_return_test() [*c]u8 {
    return &u8_array_list;
  }

  pub fn cpointer_u8_list_return_test() [*c]u8 {
    return &u8_array_list;
  }

  var result_struct = TestStruct{.value = 47};

  pub fn cpointer_struct_return_test() [*c]TestStruct {
    return &result_struct;
  }

  var struct_list: [2][*c]TestStruct = .{&result_struct, null};

  pub fn cpointer_struct_list_return_test() [*c][*c]TestStruct {
    return &struct_list;
  }

  pub fn cpointer_null_return_test() [*c]TestStruct {
    return null;
  }
  """

  describe "when returning a cpointer" do
    # we can guess what the correct cpointer should be based on the
    # type, in some cases.

    test "a u8 will be marshalled from a null terminated binary" do
      assert "abc" == cpointer_u8_return_test()
    end

    test "a u8 can be marshalled into a charlist instead" do
      assert ~C'abc' == cpointer_u8_list_return_test()
    end

    test "a struct can be marshalled into a struct" do
      assert [%{value: 47}] == cpointer_struct_list_return_test()
    end

    test "a struct pointer list can be null terminated" do
      assert %{value: 47} == cpointer_struct_return_test()
    end

    test "null can be returned" do
      assert is_nil(cpointer_null_return_test())
    end
  end
end
