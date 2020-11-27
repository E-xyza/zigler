defmodule ZiglerTest.Integration.Strategies.ThreadedNifTest do

  use ExUnit.Case, async: true

  @tag skip: true
  test "complete tests for threaded nifs"

  use Zigler

  ~Z"""
  /// nif: threaded_forty_seven/0 threaded
  fn threaded_forty_seven() i32 {
    // sleep for 2 seconds
    std.time.sleep(2000000000);
    return 47;
  }
  """

  test "threaded nifs can sleep for a while" do
    start = DateTime.utc_now
    assert 47 == threaded_forty_seven()
    elapsed = DateTime.utc_now |> DateTime.diff(start)
    assert elapsed >= 2 and elapsed < 4
  end

#  ~Z"""
#  /// nif: threaded_void/0 threaded
#  fn threaded_void() void {
#    // sleep for 50 ms
#    std.time.sleep(50000000);
#  }
#  """
#
#  test "threaded nifs can have a void return" do
#    test_pid = self()
#    spawn(fn ->
#      threaded_void()
#      send(test_pid, :done)
#    end)
#    refute_receive :done, 25
#    assert_receive :done
#  end
#
#  ~Z"""
#  /// nif: threaded_sum/1 threaded
#  fn threaded_sum(list: []i64) i64 {
#    var result : i64 = 0;
#    for (list) | val | { result += val; }
#    return result;
#  }
#  """
#
#  test "threaded nifs can have an slice input" do
#    assert 5050 == 1..100 |> Enum.to_list |> threaded_sum
#  end
#
#  ~Z"""
#  /// nif: threaded_string/1 threaded
#  fn threaded_string(str: []u8) usize {
#    return str.len;
#  }
#  """
#
#  test "threaded nifs can have an string input" do
#    assert 6 = threaded_string("foobar")
#  end

end
