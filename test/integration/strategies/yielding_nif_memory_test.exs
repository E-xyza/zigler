defmodule ZiglerTest.Integration.Strategies.YieldingNifMemoryTest do
  # make memory tests non-async
  use ExUnit.Case
  use Zig, link_libc: true

  @moduletag :yielding

  @one_m 1024 * 1024

  ~z"""
  const tenth_ms_in_us = 100_000;

  /// nif: cancellation/1 yielding
  fn cancellation(env: beam.env, pid: beam.pid) void {
    var mem = beam.allocator.alloc(u8, 10 * #{@one_m}) catch unreachable;
    defer beam.allocator.free(mem);

    _ = beam.send(env, pid, beam.make_atom(env, "allocated"));

    while (true) {
      std.time.sleep(tenth_ms_in_us);
        beam.yield() catch return;
    }
  }
  """

  test "killing the other process lets you cancel the nif" do
    test_pid = self()
    pre_memory = :erlang.memory()[:total]

    nif_pid = spawn(fn -> cancellation(test_pid) end)

    assert_receive :allocated

    trans_memory = :erlang.memory()[:total]
    assert div(trans_memory - pre_memory, @one_m) > 8

    Process.exit(nif_pid, :kill)
    Process.sleep(200)

    refute Process.alive?(nif_pid)
    post_memory = :erlang.memory()[:total]

    assert div(post_memory - pre_memory, @one_m) <= 1
  end
end
