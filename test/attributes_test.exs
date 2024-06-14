defmodule ZiglerTest.AttributesTest do
  use ExUnit.Case, async: true
  use Zig, otp_app: :zigler

  @null_attribute nil
  @bool_attribute true
  @integer_attribute 47
  @float_attribute 47.0
  @string_attribute "47"
  @tuple_attribute {:ok, 47}

  ~Z"""
  const attributes = @import("attributes");
  pub fn null_attribute() ?u32 { return attributes.null_attribute; }
  pub fn bool_attribute() bool { return attributes.bool_attribute; }
  pub fn integer_attribute() u32 { return attributes.integer_attribute; }
  pub fn float_attribute() f32 { return attributes.float_attribute; }
  pub fn string_attribute() @TypeOf(attributes.string_attribute) { return attributes.string_attribute; }
  pub fn tuple_attribute() u32 { return attributes.tuple_attribute[1]; }
  """

  test "null attribute" do
    refute null_attribute()
  end

  test "bool attribute" do
    assert true = bool_attribute()
  end

  test "integer attribute" do
    assert 47 = integer_attribute()
  end

  test "float attribute" do
    assert 47.0 = float_attribute()
  end

  test "string attribute" do
    assert "47" = string_attribute()
  end

  test "tuple attribute" do
    assert 47 = tuple_attribute()
  end
end
