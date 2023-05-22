defmodule ZiglerTest.Integration.Strategies.YieldingNifTest do
  # need to do this manually in order to prevent some strange library-on-load
  # segfault.

  use ExUnit.Case, async: true
  use Zig

  @moduletag :integration
  @moduletag :yielding

  ~Z"""
  const tenth_ms = 100; // in usec.
  const intervals = 20000;
  const USEC = e.ERL_NIF_USEC;

  /// nif: yielding_forty_seven/0 yielding
  fn yielding_forty_seven() i32 {
    var start_time = e.enif_monotonic_time(USEC);
    var this_time: e.ErlNifTime = undefined;
    var elapsed_time: e.ErlNifTime = undefined;

    // try to sleep for 2 seconds
    var idx : i32 = 0;
    while (idx < intervals) {
      this_time = e.enif_monotonic_time(USEC);
      elapsed_time = this_time - start_time;

      if (elapsed_time < 100) {
        // note that sleep is in ns.
        std.time.sleep(@intCast(u64, (100 - elapsed_time) * 1000));
      }

      start_time = this_time;
      beam.yield() catch return 0;
      idx += 1;
    }
    return 47;
  }
  """

  test "yielding nifs can sleep for a while" do
    # ideally, append `+C multi_time_warp` to `ERL_FLAGS`
    # for more info, read:
    # https://learnyousomeerlang.com/time
    # https://erlang.org/doc/apps/erts/time_correction.html
    # https://erlang.org/doc/man/erlang.html#type-time_unit
    start = System.monotonic_time(:millisecond)
    assert 47 == yielding_forty_seven()
    stop = System.monotonic_time(:millisecond)
    # NB System.monotonic_time/1 may return negative values
    # but they will always be monotonically increasing
    elapsed = abs(start - stop)
    assert elapsed >= 1000
  end

  ~Z"""
  /// nif: nonyielding_nif/0
  fn nonyielding_nif() i32 {
    beam.yield() catch return 0;
    return 47;
  }
  """

  test "yielding nif code can be run in a nonyielding fn" do
    assert 47 == nonyielding_nif()
  end

  ~Z"""
  /// nif: non_yielding_forty_seven/1 yielding
  fn non_yielding_forty_seven(yields: bool) i32 {
    if (yields) { beam.yield() catch return 0; }
    return 47;
  }

  /// nif: not_even_async/0 yielding
  fn not_even_async() i32 {
    return 47;
  }
  """

  test "yielding nifs don't have to suspend" do
    assert 47 == non_yielding_forty_seven(false)
    assert 47 == not_even_async()
  end

  ~Z"""
  /// nif: yielding_void/1 yielding
  fn yielding_void(env: beam.env, parent: beam.pid) void {
    // do at least one suspend
    beam.yield() catch return;

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
      beam.yield() catch return 0;
    }
    return result;
  }
  """

  test "yielding nifs can have an slice input" do
    assert 5050 == 1..100 |> Enum.to_list() |> yielding_sum
  end

  ~Z"""
  /// nif: yielding_string/1 yielding
  fn yielding_string(str: []u8) usize {
    beam.yield() catch return 0;
    return str.len;
  }
  """

  test "yielding nifs can have an string input" do
    assert 6 = yielding_string("foobar")
  end

  # TODO: this causes a core dumped
  @tag :skip
  test "if you pass an incorrect value in you get fce" do
    assert_raise FunctionClauseError, fn ->
      yielding_string(:foobar)
    end
  end
end
