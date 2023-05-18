defmodule ZiglerTest.Concurrency.ThreadedYieldingManualTest do
  # this is a semi manual implementation of a threaded nif.  In order to do
  # what it does, it defers all of what it does to the beam.threading namespace.
  # this includes the thread object, locking primitives, state management, etc.
  # it does, however, create and manage the Thread resource by itself.

  use ZiglerTest.IntegrationCase, async: true

  use Zig, otp_app: :zigler, cleanup: false, resources: [:ThreadResource]

  ~Z"""
  const beam = @import("beam");
  const std = @import("std");

  const Thread = beam.Thread(@TypeOf(thread));
  pub const ThreadResource = beam.Resource(*Thread, @import("root"), .{
    .Callbacks = beam.threads.ThreadCallbacks(*Thread)
  });

  fn thread(env: beam.env, pid: beam.pid) void {
    _ = beam.send(env, pid, beam.make(env, .done, .{})) catch unreachable;
  }

  fn thread_unpack(args: beam.Payload(thread)) void {
    thread(args[0], args[1]);
  }

  pub fn launch(env: beam.env, pid: beam.pid) !beam.term {
    return Thread.launch(ThreadResource, env, thread_unpack, .{env, pid});
  }
  """

  test "threaded function" do
    this = self()

    spawn(fn ->
      launch(this)
    end)

    assert_receive :done, 500
  end
end
