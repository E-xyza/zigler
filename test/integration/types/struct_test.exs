defmodule ZiglerTest.Types.StructTest do
  use ZiglerTest.IntegrationCase, async: true

  @moduletag :skip
  test "restore this!"

  #use Zig,
  #  otp_app: :zigler
#
  #~Z"""
  #pub const TestStruct = struct {
  #  value: u64
  #};
#
  #pub fn struct_test(s: TestStruct) TestStruct {
  #  return .{.value = s.value + 1};
  #}
  #"""
#
  #describe "for a basic struct" do
  #  test "can be called as a map" do
  #    assert %{value: 48} == struct_test(%{value: 47})
  #  end
#
  #  test "can be called as a keyword list" do
  #    assert %{value: 48} == struct_test(value: 47)
  #  end
#
  #  test "extraneous values in a map are ignored and tolerated" do
  #    assert %{value: 48} == struct_test(%{value: 47, foo: "bar"})
  #  end
#
  #  @tag :skip
  #  test "extraneous values in a keyword list are ignored and tolerated" do
  #    assert %{value: 48} == struct_test(%{value: 47, foo: "bar"})
  #  end
#
  #  test "missing required values in a map are not tolerated" do
  #    assert_raise ArgumentError,
  #                 "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: map | keyword (for `TestStruct`)\n     got: `%{}`\n     note: TestStruct requires the field `:value`, which is missing.)\n",
  #                 fn -> struct_test(%{}) end
  #  end
#
  #  test "missing required values in a keyword list are not tolerated" do
  #    assert_raise ArgumentError,
  #                 "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: map | keyword (for `TestStruct`)\n     got: `[]`\n     note: TestStruct requires the field `:value`, which is missing.)\n",
  #                 fn -> struct_test([]) end
  #  end
#
  #  test "incorrect value types in a map are not tolerated" do
  #    assert_raise ArgumentError,
  #                 "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: map | keyword (for `TestStruct`)\n     got: `%{value: \"foo\"}`\n     in field `:value`:\n     | expected: integer (for `u64`)\n     | got: `\"foo\"`\n",
  #                 fn -> struct_test(%{value: "foo"}) end
  #  end
#
  #  test "incorrect value types in a keyword list are not tolerated" do
  #    assert_raise ArgumentError,
  #                 "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: map | keyword (for `TestStruct`)\n     got: `[value: \"foo\"]`\n     in field `:value`:\n     | expected: integer (for `u64`)\n     | got: `\"foo\"`\n",
  #                 fn -> struct_test(value: "foo") end
  #  end
  #end
#
  #~Z"""
  #pub const default_struct = struct {
  #  value: u64 = 47
  #};
#
  #pub fn default_struct_test(s: default_struct) default_struct {
  #  return .{.value = s.value + 1};
  #}
  #"""
#
  #describe "for a struct with a default" do
  #  test "can be called as a map" do
  #    assert %{value: 48} == default_struct_test(%{value: 47})
  #  end
#
  #  test "can be called as a keyword list" do
  #    assert %{value: 48} == default_struct_test(value: 47)
  #  end
#
  #  test "extraneous values in a map are ignored and tolerated" do
  #    assert %{value: 48} == default_struct_test(%{value: 47, foo: "bar"})
  #  end
#
  #  test "extraneous values in a keyword list are ignored and tolerated" do
  #    assert %{value: 48} == default_struct_test(%{value: 47, foo: "bar"})
  #  end
#
  #  test "missing default values in a map are tolerated" do
  #    assert %{value: 48} == default_struct_test(%{})
  #  end
#
  #  test "missing default values in a keyword list are tolerated" do
  #    assert %{value: 48} == default_struct_test([])
  #  end
#
  #  test "incorrect value types in a map are not tolerated" do
  #    assert_raise ArgumentError,
  #                 "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: map | keyword (for `default_struct`)\n     got: `%{value: \"foo\"}`\n     in field `:value`:\n     | expected: integer (for `u64`)\n     | got: `\"foo\"`\n",
  #                 fn -> default_struct_test(%{value: "foo"}) end
  #  end
#
  #  test "incorrect value types in a keyword list are not tolerated" do
  #    assert_raise ArgumentError,
  #                 "errors were found at the given arguments:\n\n  * 1st argument: \n\n     expected: map | keyword (for `default_struct`)\n     got: `[value: \"foo\"]`\n     in field `:value`:\n     | expected: integer (for `u64`)\n     | got: `\"foo\"`\n",
  #                 fn -> default_struct_test(value: "foo") end
  #  end
  #end
#
  #~Z"""
  #pub fn mutable_struct_test(s: *TestStruct) *TestStruct {
  #  s.value += 1;
  #  return s;
  #}
  #"""
#
  #describe "structs can be pseudo-mutable" do
  #  test "called as a map" do
  #    assert %{value: 48} == mutable_struct_test(%{value: 47})
  #  end
  #end
end
