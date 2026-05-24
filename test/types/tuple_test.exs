defmodule ZiglerTest.Types.TupleTest do
  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler

  ~Z"""
  const beam = @import("beam");

  pub fn sum_point(point: struct { i32, i32 }) i32 {
      return point[0] + point[1];
  }

  pub fn get_first(pair: struct { beam.term, beam.term }) beam.term {
      return pair[0];
  }

  pub fn mixed_tuple(data: struct { i32, []const u8, bool }) beam.term {
      const num = data[0];
      const str = data[1];
      const flag = data[2];

      if (flag) {
          return beam.make(.{ num, str }, .{});
      } else {
          return beam.make(.{ str, num }, .{});
      }
  }
  """

  test "can receive a simple numeric tuple" do
    assert 47 = sum_point({20, 27})
  end

  test "can receive tuple with beam.term elements" do
    assert :hello = get_first({:hello, :world})
    assert 42 = get_first({42, "test"})
  end

  test "can receive mixed type tuple" do
    assert {10, "hello"} = mixed_tuple({10, "hello", true})
    assert {"hello", 10} = mixed_tuple({10, "hello", false})
  end

  test "raises on wrong arity with helpful message" do
    error = assert_raise ArgumentError, fn ->
      sum_point({1, 2, 3})
    end

    assert error.message =~ "expected: tuple"
    assert error.message =~ "expected tuple of size 2, got 3"
  end

  test "raises on wrong element type with index" do
    error = assert_raise ArgumentError, fn ->
      sum_point({1.5, 2.5})
    end

    assert error.message =~ "expected: tuple"
    assert error.message =~ "in tuple element 0:"
    assert error.message =~ "expected: integer"
  end

  test "raises on non-tuple" do
    error = assert_raise ArgumentError, fn ->
      sum_point([1, 2])
    end

    assert error.message =~ "expected: tuple"
  end

  test "raises on wrong element type at specific index" do
    error = assert_raise ArgumentError, fn ->
      mixed_tuple({10, 123, true})
    end

    assert error.message =~ "in tuple element 1:"
  end
end
