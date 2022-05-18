defmodule ZiglerTest.Integration.Strategies.ThreadedNifMemoryTest do
  # make memory tests non-async
  use ExUnit.Case
  use Zig, link_libc: true

  @moduletag :threaded

  ~Z"""
  /// nif: threaded_with_yield_cancel/1 threaded
  fn threaded_with_yield_cancel(env: beam.env, pid: beam.pid) !void {
    var leak = try beam.allocator.alloc(u8, 10_000_000);
    defer {
      _ = beam.send(env, pid, beam.make_atom(env, "done"));
      beam.allocator.free(leak);
    }

    _ = beam.send(env, pid, beam.make_atom(env, "started"));

    while (true) {
      std.time.sleep(10_000);
      try beam.yield();
    }
  }
  """

  test "threaded function can be cancelled" do
    start_memory = :erlang.memory()[:total]
    this = self()
    child = spawn(fn -> threaded_with_yield_cancel(this) end)
    assert_receive :started
    mid_memory = :erlang.memory()[:total]

    assert mid_memory - start_memory > 8_000_000

    Process.exit(child, :kill)

    assert_receive :done

    Process.sleep(100)

    final_memory = :erlang.memory()[:total]
    assert mid_memory - final_memory > 8_000_000
  end

  ~Z"""
  /// nif: threaded_with_abandonment/1 threaded
  fn threaded_with_abandonment(env: beam.env, pid: beam.pid) !void {
    var leak = try beam.allocator.alloc(u8, 10_000_000);
    defer {
      _ = beam.send(env, pid, beam.make_atom(env, "done"));
      beam.allocator.free(leak);
    }

    _ = beam.send(env, pid, beam.make_atom(env, "started"));

    std.time.sleep(1_000_000_000);
    try beam.yield();
  }
  """

  test "threaded function can be abandoned" do
    start_memory = :erlang.memory()[:total]
    this = self()
    child = spawn(fn -> threaded_with_abandonment(this) end)
    assert_receive :started
    mid_memory = :erlang.memory()[:total]

    assert mid_memory - start_memory > 8_000_000

    Process.exit(child, :kill)

    refute_receive :done

    Process.sleep(1000)

    assert_receive :done

    final_memory = :erlang.memory()[:total]
    assert mid_memory - final_memory > 8_000_000
  end
end
