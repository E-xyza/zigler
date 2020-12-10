defmodule ZiglerTest.Integration.Strategies.ThreadedNifTest do

  use ExUnit.Case, async: true
  use Zigler

  @moduletag :threaded

  ~Z"""
  /// nif: threaded_forty_seven/0 threaded
  fn threaded_forty_seven() i32 {
    // sleep for 2 seconds
    std.time.sleep(2000000000);
    return 47;
  }
  """

  test "threaded nifs can sleep for a while" do
    IO.puts("18")
    start = DateTime.utc_now
    assert 47 == threaded_forty_seven()
    elapsed = DateTime.utc_now |> DateTime.diff(start)
    assert elapsed >= 2 and elapsed < 4

    verify_cleanup()
  end

  ~Z"""
  /// nif: threaded_void/1 threaded
  fn threaded_void(env: beam.env, parent: beam.pid) void {
    // sleep for 50 ms
    std.time.sleep(50000000);

    _ = beam.send(env, parent, beam.make_atom(env, "threaded"));
  }
  """

  test "threaded nifs can have a void return and parameters" do
    IO.puts("38")
    assert :ok = threaded_void(self())
    assert_receive :threaded

    verify_cleanup()
  end

  ~Z"""
  /// nif: threaded_sum/1 threaded
  fn threaded_sum(list: []i64) i64 {
    var result : i64 = 0;
    for (list) | val | { result += val; }
    return result;
  }
  """

  test "threaded nifs can have an slice input" do
    IO.puts("55")
    assert 5050 == 1..100 |> Enum.to_list |> threaded_sum

    verify_cleanup()
  end

  ~Z"""
  /// nif: threaded_string/1 threaded
  fn threaded_string(str: []u8) usize {
    return str.len;
  }
  """

  test "threaded nifs can have an string input" do
    IO.puts("69")
    assert 6 = threaded_string("foobar")

    verify_cleanup()
  end

  test "if you pass an incorrect value in you get fce" do
    IO.puts("76")
    assert_raise FunctionClauseError, fn ->
      threaded_string(:foobar)
    end

    verify_cleanup()
  end

  def verify_cleanup do
    :erlang.garbage_collect(self())
    # debug message from the cleanup process
    assert_receive :thread_freed
  end
end
