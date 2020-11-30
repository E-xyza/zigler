 defmodule ZiglerTest.Integration.Strategies.YieldingNifTest do

  use ExUnit.Case, async: true

  use Zigler

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
    assert elapsed >= 2 and elapsed < 4
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
end
