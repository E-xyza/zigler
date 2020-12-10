defmodule CiTest do

  use ExUnit.Case, async: true
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
end
