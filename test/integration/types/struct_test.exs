defmodule ZiglerTest.Types.StructTest do
  use ExUnit.Case, async: true

  use Zig,
    otp_app: :zigler,
    local_zig: true

  # TODO: validations that the structs are defined "pub"

  ~Z"""
  pub const basic_struct = struct {
    value: u64
  };

  pub fn struct_test(s: basic_struct) basic_struct {
    return .{.value = s.value + 1};
  }
  """

  describe "for a basic struct" do
    test "can be called as a map" do
      assert %{value: 48} == struct_test(%{value: 47})
    end

    test "can be called as a keyword list" do
      assert %{value: 48} == struct_test(value: 47)
    end

    test "extraneous values in a map are ignored and tolerated" do
      assert %{value: 48} == struct_test(%{value: 47, foo: "bar"})
    end

    test "extraneous values in a keyword list are ignored and tolerated" do
      assert %{value: 48} == struct_test(%{value: 47, foo: "bar"})
    end

    test "missing required values in a map are not tolerated" do
      assert_raise ArgumentError, "", fn -> struct_test(%{}) end
    end

    test "missing required values in a keyword list are not tolerated" do
      assert_raise ArgumentError, "", struct_test([])
    end

    test "incorrect value types in a map are not tolerated" do
      assert_raise ArgumentError, "", fn -> struct_test(%{value: "foo"}) end
    end

    test "incorrect value types in a keyword list are not tolerated" do
      assert_raise ArgumentError, "", fn -> struct_test(value: "foo") end
    end
  end

  ~Z"""
  pub const default_struct = struct {
    value: u64 = 47
  };

  pub fn default_struct_test(s: default_struct) default_struct {
    return .{.value = s.value + 1};
  }
  """

  describe "for a struct with a default" do
    test "can be called as a map" do
      assert %{value: 48} == default_struct_test(%{value: 47})
    end

    test "can be called as a keyword list" do
      assert %{value: 48} == default_struct_test(value: 47)
    end

    test "extraneous values in a map are ignored and tolerated" do
      assert %{value: 48} == default_struct_test(%{value: 47, foo: "bar"})
    end

    test "extraneous values in a keyword list are ignored and tolerated" do
      assert %{value: 48} == default_struct_test(%{value: 47, foo: "bar"})
    end

    test "missing default values in a map are tolerated" do
      assert %{value: 48} == default_struct_test(%{})
    end

    test "missing default values in a keyword list are tolerated" do
      assert %{value: 48} == default_struct_test([])
    end

    test "incorrect value types in a map are not tolerated" do
      assert_raise ArgumentError, "", fn -> default_struct_test(%{value: "foo"}) end
    end

    test "incorrect value types in a keyword list are not tolerated" do
      assert_raise ArgumentError, "", fn -> default_struct_test(value: "foo") end
    end
  end

  ~Z"""
  pub const packed_struct = packed struct {
    flag: bool = false,
    name: [3]u8,
    thing: u5
  };

  pub fn packed_struct_test(s: packed_struct) packed_struct {
    var result = s;
    result.flag = !s.flag;
    return result;
  }
  """

  describe "a packed struct" do
    test "can be delivered as a bitstring" do
      assert %{flag: false, thing: 15, name: "foo"} =
               packed_struct_test(<<15::5, "oof"::binary, 1::1>>)
    end
  end
end
