 defmodule ZiglerTest.Integration.Strategies.YieldingNifTest do

  use ExUnit.Case, async: true
  use Zigler

  @moduletag :yielding

  ~Z"""
  const tenth_millisecond = 100000;
  const intervals = 20000;

  /// nif: yielding_forty_seven/0 yielding
  fn yielding_forty_seven() i32 {
    // sleep for 2 seconds
    var idx : i32 = 0;
    while (idx < intervals) {
      std.time.sleep(tenth_millisecond);
      idx += 1;
      suspend{ _ = beam.yield() catch return 0; }
    }
    return 47;
  }
  """

  test "yielding nifs can sleep for a while" do
    start = DateTime.utc_now
    assert 47 == yielding_forty_seven()
    elapsed = DateTime.utc_now |> DateTime.diff(start)
    assert elapsed >= 2
    assert elapsed < 4
  end

  ~Z"""
  /// nif: non_yielding_forty_seven/1 yielding
  fn non_yielding_forty_seven(yields: bool) i32 {
    if (yields) { _ = beam.yield() catch return 0; }
    return 47;
  }
  """

  test "yielding nifs don't have to suspend" do
    assert 47 == non_yielding_forty_seven(false)
  end

  ~Z"""
  /// nif: yielding_void/1 yielding
  fn yielding_void(env: beam.env, parent: beam.pid) void {
    // do at least one suspend
    suspend { _ = beam.yield() catch return; }

    _ = beam.send(env, parent, beam.make_atom(env, "yielding"));
  }
  """

  test "yielding nifs can have a void return and parameters" do
    assert :ok = yielding_void(self())
    assert_receive :yielding
  end

  ~Z"""
  /// nif: yielding_sum/1 yielding
  fn yielding_sum(list: []i64) i64 {
    var result : i64 = 0;
    for (list) | val | {
      result += val;
      suspend { _ = beam.yield() catch return 0; }
    }
    return result;
  }
  """

  test "yielding nifs can have an slice input" do
    assert 5050 == 1..100 |> Enum.to_list |> yielding_sum
  end

  ~Z"""
  /// nif: yielding_string/1 yielding
  fn yielding_string(str: []u8) usize {
    suspend { _ = beam.yield() catch return 0; }
    return str.len;
  }
  """

  test "yielding nifs can have an string input" do
    assert 6 = yielding_string("foobar")
  end

  test "if you pass an incorrect value in you get fce" do
    assert_raise FunctionClauseError, fn ->
      yielding_string(:foobar)
    end
  end
end
