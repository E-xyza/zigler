defmodule ZiglerTest.Integration.Strategies.DirtyNifTest do
  use ExUnit.Case, async: true

  use Zig

  ~Z"""
  /// nif: dirty_cpu_forty_seven/0 dirty_cpu
  fn dirty_cpu_forty_seven() i32 {
    // sleep for 2 seconds
    std.time.sleep(2000000000);
    return 47;
  }
  """

  test "dirty_cpu nifs can sleep for a while" do
    start = DateTime.utc_now()
    assert 47 == dirty_cpu_forty_seven()
    elapsed = DateTime.utc_now() |> DateTime.diff(start)
    assert elapsed >= 2 and elapsed < 4
  end

  ~Z"""
  /// nif: dirty_cpu_forty_seven_yielding/0 dirty_cpu
  fn dirty_cpu_forty_seven_yielding() i32 {
    // this yield no-ops:
    beam.yield() catch return 0;
    return 47;
  }
  """

  test "yielding functions can be called as dirty_cpu" do
    assert 47 == dirty_cpu_forty_seven_yielding()
  end

  ~Z"""
  /// nif: dirty_io_forty_seven/0 dirty_io
  fn dirty_io_forty_seven() i32 {
    // sleep for 2 seconds
    std.time.sleep(2000000000);
    return 47;
  }
  """

  test "dirty_io nifs can sleep for a while" do
    start = DateTime.utc_now()
    assert 47 == dirty_io_forty_seven()
    elapsed = DateTime.utc_now() |> DateTime.diff(start)
    assert elapsed >= 2 and elapsed < 4
  end

  ~Z"""
  /// nif: dirty_io_forty_seven_yielding/0 dirty_io
  fn dirty_io_forty_seven_yielding() i32 {
    // this yield no-ops:
    beam.yield() catch return 0;
    return 47;
  }
  """

  test "yielding functions can be called as dirty_io" do
    assert 47 == dirty_io_forty_seven_yielding()
  end
end
